open Lwt.Syntax
include Irmin_query_intf

module Make (X : Irmin.Generic_key.S) = struct
  module Store = X

  type 'a with_options =
    ?depth:Store.Tree.depth ->
    ?prefix:Store.Path.t ->
    ?limit:int ->
    ?order:[ `Random of Random.State.t | `Sorted | `Undefined ] ->
    'a

  let rec combine_paths prefix k =
    match Store.Path.decons prefix with
    | Some (step, path) -> combine_paths path (Store.Path.cons step k)
    | None -> k

  let contents ?depth ?prefix ?limit ?order store :
      (Store.path * Store.contents) Lwt_seq.t Lwt.t =
    let* prefix, tree =
      match prefix with
      | Some prefix ->
          let+ t = Store.get_tree store prefix in
          (prefix, t)
      | None ->
          let+ t = Store.tree store in
          (Store.Path.empty, t)
    in
    let exception Return of (Store.path * Store.contents) Lwt_seq.t in
    let count = ref 0 in
    let limit_n = match limit with Some x -> x | None -> 0 in
    let contents path c acc =
      if Option.is_some limit && !count >= limit_n then raise (Return acc)
      else
        let () = incr count in
        Lwt.return (Lwt_seq.cons (combine_paths prefix path, c) acc)
    in
    Lwt.catch
      (fun () -> Store.Tree.fold ?order ?depth tree ~contents Lwt_seq.empty)
      (function Return acc -> Lwt.return acc | exn -> raise exn)

  let tree ?depth ?prefix ?limit ?order store :
      (Store.path * Store.tree) Lwt_seq.t Lwt.t =
    let* prefix, tree' =
      match prefix with
      | Some prefix ->
          let+ t = Store.get_tree store prefix in
          (prefix, t)
      | None ->
          let+ t = Store.tree store in
          (Store.Path.empty, t)
    in
    let exception Return of (Store.path * Store.tree) Lwt_seq.t in
    let count = ref 0 in
    let limit_n = match limit with Some x -> x | None -> 0 in
    let tree path tr acc =
      if Option.is_some limit && !count >= limit_n then raise (Return acc)
      else
        let () = incr count in
        match Store.Tree.inspect tr with
        | `Contents -> Lwt.return acc
        | `Node _ ->
            Lwt.return (Lwt_seq.cons (combine_paths prefix path, tr) acc)
    in
    Lwt.catch
      (fun () -> Store.Tree.fold ?order ?depth tree' ~tree Lwt_seq.empty)
      (function Return acc -> Lwt.return acc | exn -> raise exn)

  let list ?depth ?prefix ?limit ?order store =
    let+ x = contents ?depth ?prefix ?limit ?order store in
    Lwt_seq.map fst x

  module Search = struct
    module Cache = struct
      include Irmin.Backend.Lru.Make (struct
        type t = Store.path * Store.hash

        let hash x =
          Irmin.Type.(unstage @@ short_hash (pair Store.path_t Store.hash_t)) x

        let equal a b =
          Irmin.Type.(unstage @@ equal (pair Store.path_t Store.hash_t)) a b
      end)

      let find_opt t h = if mem t h then Some (find t h) else None
    end

    type 'a f = Store.path -> Store.contents -> 'a option Lwt.t
    type 'a t = { cache : 'a option Cache.t option; f : 'a f }

    let v ?(cache = true) f =
      let cache = if cache then Some (Cache.create 16) else None in
      { cache; f }

    let f { f; _ } = f
    let reset { cache; _ } = Option.iter Cache.clear cache

    let exec (type a) ?depth ?prefix ?limit ?order (t : a t) store =
      let f = f t in
      let inner (k, v) : a option Lwt.t =
        match t.cache with
        | Some cache -> (
            let hash = Store.Contents.hash v in
            match Cache.find_opt cache (k, hash) with
            | Some x -> Lwt.return x
            | None ->
                let+ x = f k v in
                Cache.add cache (k, hash) x;
                x)
        | None -> f k v
      in
      let+ (items : (Store.path * Store.contents) Lwt_seq.t) =
        contents ?depth ?prefix ?limit ?order store
      in
      Lwt_seq.filter_map_s inner items
  end

  module Expr = struct
    type 'a t =
      | Find : Store.path t -> Store.contents option t
      | Find_tree : Store.path t -> Store.tree option t
      | Remove : Store.path t -> unit t
      | Set : Store.path t * Store.contents t -> unit t
      | Set_tree : Store.path t * Store.tree t -> unit t
      | Value : 'a -> 'a t
      | Bind : ('b t * ('b -> 'a t Lwt.t)) -> 'a t
      | Group : x list -> unit t

    and x = X : 'a t -> x
    and group = x list

    let find path = Find path
    let find_tree path = Find_tree path
    let remove path = Remove path
    let set path value = Set (path, value)
    let set_tree path value = Set_tree (path, value)
    let value x = Value x
    let bind f t = Bind (t, f)
    let empty = []
    let add g a = X a :: g
    let group g = Group g

    let map f t =
      Bind
        ( t,
          fun x ->
            let+ x = f x in
            value x )

    let path p = value p
    let ( let& ) t f = bind f t
    let ( let| ) t f = map f t

    let get path =
      let| a = find path in
      Lwt.return @@ Option.get a

    let get_tree path =
      let| a = find_tree path in
      Lwt.return @@ Option.get a

    let list t = map (fun tree -> Store.Tree.list tree Store.Path.empty) t

    let tree_pair tree x =
      let open Lwt.Syntax in
      let+ x in
      (tree, x)

    let rec eval' : type a. Store.tree -> a t -> (Store.tree * a) Lwt.t =
     fun tree expr ->
      let open Lwt.Syntax in
      match expr with
      | Find path ->
          let* tree, path = eval' tree path in
          tree_pair tree @@ Store.Tree.find tree path
      | Find_tree path ->
          let* tree, path = eval' tree path in
          tree_pair tree @@ Store.Tree.find_tree tree path
      | Remove path ->
          let* tree, path = eval' tree path in
          let+ tree = Store.Tree.remove tree path in
          (tree, ())
      | Set (path, value) ->
          let* tree, path = eval' tree path in
          let* tree, value = eval' tree value in
          let+ tree = Store.Tree.add tree path value in
          (tree, ())
      | Set_tree (path, t) ->
          let* tree, path = eval' tree path in
          let* tree, t = eval' tree t in
          let+ tree = Store.Tree.add_tree tree path t in
          (tree, ())
      | Value x -> Lwt.return (tree, x)
      | Bind (x, f) ->
          let* tree, x = eval' tree x in
          let* x = f x in
          eval' tree x
      | Group l ->
          let+ tree =
            Lwt_list.fold_left_s
              (fun tree (X x) ->
                let+ tree, _x = eval' tree x in
                tree)
              tree l
          in
          (tree, ())

    let eval_tree expr tree = eval' tree expr

    let eval ?parents ?(path = Store.Path.empty) ~info store expr =
      let open Lwt.Syntax in
      let ret = ref None in
      let+ () =
        Store.with_tree_exn ~info ?parents store path (fun tree ->
            let tree = Option.value ~default:(Store.Tree.empty ()) tree in
            let+ tree, x = eval' tree expr in
            ret := Some x;
            Some tree)
      in
      Option.get !ret

    let eval_readonly ?(path = Store.Path.empty) store expr =
      let open Lwt.Syntax in
      let* tree = Store.find_tree store path in
      let tree = Option.value ~default:(Store.Tree.empty ()) tree in
      let+ _, x = eval' tree expr in
      x
  end
end

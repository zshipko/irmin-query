open Lwt.Syntax
include Irmin_query_intf

module Make (X : Irmin.Generic_key.S) = struct
  module Store = X

  module Cache = struct
    include Irmin.Backend.Lru.Make (struct
      type t = Store.hash

      let hash x = Irmin.Type.(unstage @@ short_hash Store.hash_t) x
      let equal a b = Irmin.Type.(unstage @@ equal Store.hash_t) a b
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

  let exec (type a) ?depth ?prefix ?limit ?order (t : a t) store =
    let f = f t in
    let inner (k, v) : a option Lwt.t =
      match t.cache with
      | Some cache -> (
          let hash = Store.Contents.hash v in
          match Cache.find_opt cache hash with
          | Some x -> Lwt.return x
          | None ->
              let+ x = f k v in
              Cache.add cache hash x;
              x)
      | None -> f k v
    in
    let+ (items : (Store.path * Store.contents) Lwt_seq.t) =
      contents ?depth ?prefix ?limit ?order store
    in
    Lwt_seq.filter_map_s inner items

  module Expr = struct
    type 'a t =
      | Find : Store.path -> Store.contents option t
      | Find_tree : Store.path -> Store.tree option t
      | Remove : Store.path -> unit t
      | Set : Store.path * Store.contents t -> unit t
      | Set_tree : Store.path * Store.tree t -> unit t
      | Value : 'a -> 'a t
      | Map : ('b t * ('b -> 'a Lwt.t)) -> 'a t

    let find path = Find path
    let find_tree path = Find_tree path
    let remove path = Remove path
    let set path value = Set (path, value)
    let set_tree path value = Set_tree (path, value)
    let value x = Value x
    let map f t = Map (t, f)
    let ( let& ) t f = map f t

    let get path =
      let& a = find path in
      Lwt.return @@ Option.get a

    let get_tree path =
      let& a = find_tree path in
      Lwt.return @@ Option.get a

    let tree_pair tree x =
      let+ x in
      (tree, x)

    let rec exec' : type a. Store.tree -> a t -> (Store.tree * a) Lwt.t =
     fun tree expr ->
      match expr with
      | Find path -> tree_pair tree @@ Store.Tree.find tree path
      | Find_tree path -> tree_pair tree @@ Store.Tree.find_tree tree path
      | Remove path ->
          let+ tree = Store.Tree.remove tree path in
          (tree, ())
      | Set (path, value) ->
          let* tree, value = exec' tree value in
          let+ tree = Store.Tree.add tree path value in
          (tree, ())
      | Set_tree (path, t) ->
          let* tree, t = exec' tree t in
          let+ tree = Store.Tree.add_tree tree path t in
          (tree, ())
      | Value x -> Lwt.return (tree, x)
      | Map (x, f) ->
          let* tree, x = exec' tree x in
          tree_pair tree (f x)

    let exec ?parents ?(path = Store.Path.empty) ~info store expr =
      let ret = ref None in
      let+ () =
        Store.with_tree_exn ~info ?parents store path (fun tree ->
            let tree = Option.value ~default:(Store.Tree.empty ()) tree in
            let+ tree, x = exec' tree expr in
            ret := Some x;
            Some tree)
      in
      Option.get !ret
  end
end

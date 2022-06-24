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

  let init store prefix limit =
    let+ prefix, tree =
      match prefix with
      | Some prefix ->
          let+ t = Store.get_tree store prefix in
          (prefix, t)
      | None ->
          let+ t = Store.tree store in
          (Store.Path.empty, t)
    in
    let count = ref 0 in
    let limit_n = match limit with Some x -> x | None -> 0 in
    (prefix, tree, limit_n, count)

  let contents ?depth ?prefix ?limit ?order store :
      (Store.path * Store.contents) Lwt_seq.t Lwt.t =
    let exception Return of (Store.path * Store.contents) Lwt_seq.t in
    let* prefix, tree, limit_n, count = init store prefix limit in
    let contents path c acc =
      if Option.is_some limit && !count >= limit_n then raise (Return acc)
      else
        let () = incr count in
        Lwt.return (Lwt_seq.cons (combine_paths prefix path, c) acc)
    in
    Lwt.catch
      (fun () -> Store.Tree.fold ?order ?depth tree ~contents Lwt_seq.empty)
      (function Return acc -> Lwt.return acc | exn -> raise exn)

  let trees ?depth ?prefix ?limit ?order store :
      (Store.path * Store.tree) Lwt_seq.t Lwt.t =
    let* prefix, tree', limit_n, count = init store prefix limit in
    let exception Return of (Store.path * Store.tree) Lwt_seq.t in
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

  let nodes ?depth ?prefix ?limit ?order store :
      (Store.path * Store.node) Lwt_seq.t Lwt.t =
    let* prefix, tree', limit_n, count = init store prefix limit in
    let exception Return of (Store.path * Store.node) Lwt_seq.t in
    let node path (node : Store.node) acc =
      if Option.is_some limit && !count >= limit_n then raise (Return acc)
      else
        let () = incr count in
        Lwt.return (Lwt_seq.cons (combine_paths prefix path, node) acc)
    in
    Lwt.catch
      (fun () -> Store.Tree.fold ?order ?depth tree' ~node Lwt_seq.empty)
      (function Return acc -> Lwt.return acc | exn -> raise exn)

  let keys ?depth ?prefix ?limit ?order store =
    let+ x = contents ?depth ?prefix ?limit ?order store in
    Lwt_seq.map fst x

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

  let select ?depth ?prefix ?limit ?order ?cache f store =
    let inner =
      match cache with
      | Some cache -> (
          fun (k, v) ->
            let hash = Store.Contents.hash v in
            match Cache.find_opt cache (k, hash) with
            | Some x -> Lwt.return x
            | None ->
                let+ x = f k v in
                Cache.add cache (k, hash) x;
                x)
      | None -> fun (k, v) -> f k v
    in
    let+ (items : (Store.path * Store.contents) Lwt_seq.t) =
      contents ?depth ?prefix ?limit ?order store
    in
    Lwt_seq.filter_map_s inner items

  let update ?depth ?prefix ?limit ?order ?parents ?strategy ~info f store =
    let path = Option.value ~default:Store.Path.empty prefix in
    let* items = contents ?depth ?prefix ?limit ?order store in
    let items =
      Lwt_seq.map_s
        (fun (k, v) ->
          let+ v = f k v in
          (k, v))
        items
    in
    Store.with_tree_exn store path
      (fun tree ->
        let tree = Option.value ~default:(Store.Tree.empty ()) tree in
        let* tree =
          Lwt_seq.fold_left_s
            (fun tree (k, v) ->
              match v with
              | Some v -> Store.Tree.add tree k v
              | None -> Store.Tree.remove tree k)
            tree items
        in
        if Store.Tree.is_empty tree then Lwt.return_none
        else Lwt.return_some tree)
      ?parents ?strategy ~info

  module Expr = struct
    type 'a t =
      | Find : Store.path t -> Store.contents option t
      | Find_tree : Store.path t -> Store.tree option t
      | Remove : Store.path t -> unit t
      | Set : Store.path t * Store.contents t -> unit t
      | Set_tree : Store.path t * Store.tree t -> unit t
      | Value : 'a -> 'a t
      | Bind : ('b t * ('b -> 'a t Lwt.t)) -> 'a t
      | Join : 'a t * 'b t -> 'b t

    let find path = Find path
    let find_tree path = Find_tree path
    let remove path = Remove path
    let set path value = Set (path, value)
    let set_tree path value = Set_tree (path, value)
    let value x = Value x
    let bind f t = Bind (t, f)
    let join a b = Join (a, b)

    let map f t =
      Bind
        ( t,
          fun x ->
            let+ x = f x in
            value x )

    let path p = value p
    let ( let& ) t f = bind f t
    let ( let| ) t f = map f t
    let ( & ) a b = join a b

    let get path =
      let| a = find path in
      Lwt.return @@ Option.get a

    let get_tree path =
      let| a = find_tree path in
      Lwt.return @@ Option.get a

    let list t = map (fun tree -> Store.Tree.list tree Store.Path.empty) t
    let rename ~from to_ = set_tree to_ (get_tree from)

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
      | Join (a, b) ->
          let* tree, _ = eval' tree a in
          eval' tree b

    let eval_tree expr tree = eval' tree expr

    let eval ?parents ?(prefix = Store.Path.empty) ~info store expr =
      let open Lwt.Syntax in
      let ret = ref None in
      let+ () =
        Store.with_tree_exn ~info ?parents store prefix (fun tree ->
            let tree = Option.value ~default:(Store.Tree.empty ()) tree in
            let+ tree, x = eval' tree expr in
            ret := Some x;
            Some tree)
      in
      Option.get !ret
  end
end

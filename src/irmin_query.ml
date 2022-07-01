open Lwt.Syntax
include Irmin_query_intf

module Make (X : Irmin.Generic_key.S) = struct
  module Store = X

  let rec _combine_paths prefix k =
    match Store.Path.decons prefix with
    | Some (step, path) -> _combine_paths path (Store.Path.cons step k)
    | None -> k

  let init store prefix =
    let+ t = Store.get_tree store prefix in
    (prefix, t)

  type kind =
    | Contents : Store.Tree.Contents.t * Store.metadata -> kind
    | Node : Store.node -> kind

  let rec seq t ?(max_depth = -1) path : (Store.Path.t * kind) Lwt_seq.t Lwt.t =
    let+ s = Store.Tree.seq t ~cache:true path in
    let s = Lwt_seq.of_seq s in
    Lwt_seq.filter_map_s
      (fun (step, t) ->
        let path' = Store.Path.rcons path step in
        match Store.Tree.destruct t with
        | `Contents (a, b) ->
            Lwt.return_some @@ Lwt_seq.return (path', Contents (a, b))
        | `Node node ->
            if max_depth = 0 then Lwt.return_none
            else
              let* x = seq ~max_depth:(max_depth - 1) t Store.Path.empty in
              let x = Lwt_seq.append (Lwt_seq.return (path', Node node)) x in
              Lwt.return_some x)
      s
    |> Lwt_seq.flat_map Fun.id

  let contents ?max_depth tree : (Store.path * Store.contents) Lwt_seq.t Lwt.t =
    let+ seq = seq ?max_depth tree Store.Path.empty in
    Lwt_seq.filter_map_s
      (fun (k, v) ->
        match v with
        | Contents (c, _) ->
            let+ c = Store.Tree.Contents.force_exn c in
            Some (k, c)
        | _ -> Lwt.return_none)
      seq

  let trees ?max_depth tree : (Store.path * Store.tree) Lwt_seq.t Lwt.t =
    let+ seq = seq ?max_depth tree Store.Path.empty in
    Lwt_seq.map_s
      (fun (k, v) ->
        match v with
        | Contents (c, metadata) ->
            let+ c = Store.Tree.Contents.force_exn c in
            (k, Store.Tree.of_contents ~metadata c)
        | Node node -> Lwt.return (k, Store.Tree.of_node node))
      seq

  let nodes ?max_depth tree : (Store.path * Store.node) Lwt_seq.t Lwt.t =
    let+ seq = seq ?max_depth tree Store.Path.empty in
    Lwt_seq.filter_map
      (fun (k, v) ->
        match v with Contents _ -> None | Node node -> Some (k, node))
      seq

  let paths ?max_depth tree : Store.path Lwt_seq.t Lwt.t =
    let+ seq = seq ?max_depth tree Store.Path.empty in
    Lwt_seq.filter_map
      (fun (k, v) -> match v with Contents _ -> Some k | _ -> None)
      seq

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

  let select ?limit ?max_depth ?cache f store prefix =
    let count = ref 0 in
    let incr_count x =
      if Option.is_some x then
        let () = incr count in
        Lwt.return x
      else Lwt.return x
    in
    let has_limit = Option.is_some limit in
    let limit_n = Option.value ~default:0 limit in
    let inner =
      match cache with
      | Some cache -> (
          fun (k, v) ->
            if has_limit && !count >= limit_n then Lwt.return_none
            else
              let hash = Store.Contents.hash v in
              match Cache.find_opt cache (k, hash) with
              | Some x -> incr_count x
              | None ->
                  let* x = f k v in
                  Cache.add cache (k, hash) x;
                  incr_count x)
      | None ->
          fun (k, v) ->
            if has_limit && !count >= limit_n then Lwt.return_none
            else
              let* x = f k v in
              incr_count x
    in
    let* _, tree = init store prefix in
    let+ (items : (Store.path * Store.contents) Lwt_seq.t) =
      contents ?max_depth tree
    in
    Lwt_seq.filter_map_s inner items

  let update ?max_depth ?parents ?strategy ~info f store prefix =
    let* prefix, tree = init store prefix in
    let* items = contents ?max_depth tree in
    let items =
      Lwt_seq.filter_map_s
        (fun (k, v) ->
          let+ v = f k v in
          match v with None -> None | Some v -> Some (k, v))
        items
    in
    Store.with_tree_exn store prefix
      (fun tree ->
        let tree = Option.value ~default:(Store.Tree.empty ()) tree in
        let* tree =
          Lwt_seq.fold_left_s
            (fun tree (k, v) ->
              match v with
              | `Set v -> Store.Tree.add tree k v
              | `Remove -> Store.Tree.remove tree k)
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

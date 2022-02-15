open Lwt.Syntax
include Irmin_query_intf

module Make (X : Irmin.S) : QUERY with module Store = X = struct
  module Store = X

  module Filter = struct
    module Cache = struct
      type t = (Store.commit, (Store.path, bool) Hashtbl.t) Hashtbl.t
    end

    type f = Store.path -> Store.contents -> bool Lwt.t
    type t = { cache : Cache.t; f : f; pure : bool }

    let v ?(pure = true) f =
      let cache = Hashtbl.create 8 in
      { cache; f; pure }

    let cache { cache; _ } = cache
    let f { f; _ } = f
    let pure { pure; _ } = pure
  end

  module Iter = struct
    module Cache = struct
      type 'a t = (Store.commit, (Store.path, 'a) Hashtbl.t) Hashtbl.t
    end

    type 'a f = Store.path -> Store.contents -> 'a Lwt.t
    type 'a t = { cache : 'a Cache.t; f : 'a f; pure : bool }

    let v ?(pure = true) f =
      let cache = Hashtbl.create 8 in
      { cache; f; pure }

    let f { f; _ } = f
    let cache { cache; _ } = cache
    let pure { pure; _ } = pure
  end

  module Settings = struct
    type t = {
      depth : Store.Tree.depth option;
      prefix : Store.Path.t option;
      root : Store.Path.t;
      limit : int option;
    }

    let default =
      { depth = None; prefix = None; root = Store.Path.empty; limit = None }
  end

  module Results = Lwt_seq

  let rec combine_paths prefix k =
    match Store.Path.decons prefix with
    | Some (step, path) -> combine_paths path (Store.Path.cons step k)
    | None -> k

  let paths ?(settings = Settings.default) store : Store.path Results.t Lwt.t =
    let* prefix, tree =
      match settings.prefix with
      | Some prefix ->
          let+ t = Store.get_tree store prefix in
          (prefix, t)
      | None ->
          let+ t = Store.tree store in
          (Store.Path.empty, t)
    in
    let contents path _ acc =
      Lwt.return (Results.cons (combine_paths prefix path) acc)
    in
    Store.Tree.fold ?depth:settings.depth tree ~contents Results.empty

  let items ?(settings = Settings.default) store :
      (Store.path * Store.contents) Results.t Lwt.t =
    let* prefix, tree =
      match settings.prefix with
      | Some prefix ->
          let+ t = Store.get_tree store prefix in
          (prefix, t)
      | None ->
          let+ t = Store.tree store in
          (Store.Path.empty, t)
    in
    let contents path c acc =
      Lwt.return (Results.cons (combine_paths prefix path, c) acc)
    in
    Store.Tree.fold ?depth:settings.depth tree ~contents Results.empty

  let iter' ?settings f store results =
    let settings =
      match settings with Some x -> x | None -> Settings.default
    in
    let limit = match settings.limit with Some x -> x | None -> 0 in
    let pure = Iter.pure f in
    let cache = Iter.cache f in
    let f = Iter.f f in
    let* head = Store.Head.get store in
    let cache : (Store.path, 'a) Hashtbl.t =
      match Hashtbl.find_opt cache head with
      | Some x -> x
      | None ->
          let ht = Hashtbl.create 8 in
          Hashtbl.replace cache head ht;
          ht
    in
    let rec inner seq count =
      let* seq = seq () in
      match seq with
      | Results.Nil -> Lwt.return Results.empty
      | Results.Cons ((k, v), seq) ->
          if limit > 0 && count >= limit then Lwt.return Results.empty
          else
            let* s = inner seq (count + 1) in
            let* x =
              match (pure, Hashtbl.find_opt cache k) with
              | true, Some x -> Lwt.return x
              | true, None ->
                  let+ x = f k v in
                  Hashtbl.replace cache k x;
                  x
              | _ -> f k v
            in
            Lwt.return @@ Results.cons x s
    in
    inner results 0

  let map (f : 'a Iter.t) ?settings store : 'a Results.t Lwt.t =
    let* (items : (Store.path * Store.contents) Results.t) =
      items ?settings store
    in
    iter' ?settings f store items

  let filter_map :
      filter:Filter.t ->
      'a Iter.t ->
      ?settings:Settings.t ->
      Store.t ->
      'a Results.t Lwt.t =
   fun ~filter f ?settings store ->
    let* head = Store.Head.get store in
    let cache = Filter.cache filter in
    let filter' = Filter.f filter in
    let filter_cache : (Store.path, bool) Hashtbl.t =
      match Hashtbl.find_opt cache head with
      | Some x -> x
      | None ->
          let ht = Hashtbl.create 8 in
          Hashtbl.replace cache head ht;
          ht
    in
    let* items = items ?settings store in
    let rec inner seq =
      let* seq = seq () in
      match seq with
      | Results.Nil -> Lwt.return Results.empty
      | Results.Cons ((k, v), seq) ->
          let* (ok : bool) =
            if not (Filter.pure filter) then filter' k v
            else
              match Hashtbl.find_opt filter_cache k with
              | Some ok -> Lwt.return ok
              | None ->
                  let+ ok = filter' k v in
                  let () = Hashtbl.replace filter_cache k ok in
                  ok
          in
          let* i = inner seq in
          if ok then Lwt.return @@ Results.cons (k, v) i else inner seq
    in
    let* x = inner items in
    iter' ?settings f store x
end

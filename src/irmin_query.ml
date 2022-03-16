open Lwt.Syntax
include Irmin_query_intf

module Make (X : Irmin.S) : S with module Store = X = struct
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
      limit : int option;
      order : [ `Random of Random.State.t | `Sorted | `Undefined ];
    }

    let default =
      { depth = None; prefix = None; limit = None; order = `Undefined }
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
    Store.Tree.fold ~order:settings.order ?depth:settings.depth tree ~contents
      Results.empty

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
    let exception Return of (Store.path * Store.contents) Results.t in
    let count = ref 0 in
    let limit = match settings.limit with Some x -> x | None -> 0 in
    let contents path c acc =
      if Option.is_some settings.limit && !count >= limit then
        raise (Return acc)
      else
        let () = incr count in
        Lwt.return (Results.cons (combine_paths prefix path, c) acc)
    in
    Lwt.catch
      (fun () ->
        Store.Tree.fold ?depth:settings.depth tree ~contents Results.empty)
      (function Return acc -> Lwt.return acc | exn -> raise exn)

  let iter' (type a) (f : a Iter.t) store
      (results : (Store.path * Store.contents) Results.t) =
    let pure = Iter.pure f in
    let cache = Iter.cache f in
    let f = Iter.f f in
    let* head = Store.Head.get store in
    let cache : (Store.path, a) Hashtbl.t =
      match Hashtbl.find_opt cache head with
      | Some x -> x
      | None ->
          let ht = Hashtbl.create 8 in
          Hashtbl.replace cache head ht;
          ht
    in
    let inner (k, v) : a option Lwt.t =
      let* x =
        match (pure, Hashtbl.find_opt cache k) with
        | true, Some x -> Lwt.return x
        | true, None ->
            let+ x = f k v in
            Hashtbl.replace cache k x;
            x
        | _ -> f k v
      in
      Lwt.return_some x
    in
    Lwt.return @@ Results.filter_map_s inner results

  let map (f : 'a Iter.t) ?settings store : 'a Results.t Lwt.t =
    let* (items : (Store.path * Store.contents) Results.t) =
      items ?settings store
    in
    iter' f store items

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
    let inner (k, v) : bool Lwt.t =
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
      Lwt.return ok
    in
    let* items = items ?settings store in
    let x = Results.filter_s inner items in
    iter' f store x

  let reduce f results init =
    Results.fold_left_s (fun acc x -> f x acc) init results
end

open Lwt.Syntax
include Irmin_query_intf

module Make (X : Irmin.S) : S with module Store = X = struct
  module Store = X

  module Cache = struct
    module Lru = struct
      include Irmin.Backend.Lru.Make (struct
        type t = Store.hash

        let hash x = Irmin.Type.(unstage @@ short_hash Store.hash_t) x
        let equal a b = Irmin.Type.(unstage @@ equal Store.hash_t) a b
      end)

      let find_opt t h = if mem t h then Some (find t h) else None
    end

    type 'a t = (Store.path, 'a option) Hashtbl.t Lru.t
  end

  type 'a f = Store.path -> Store.contents -> 'a option Lwt.t
  type 'a t = { cache : 'a Cache.t; f : 'a f; enable_cache : bool }

  let v ?(cache = true) f =
    let enable_cache = cache in
    let cache = Cache.Lru.create 16 in
    { cache; f; enable_cache }

  let f { f; _ } = f
  let cache { cache; _ } = cache
  let enable_cache { enable_cache; _ } = enable_cache
  let reset { cache; _ } = Cache.Lru.clear cache

  module Options = struct
    type t = {
      depth : Store.Tree.depth option;
      prefix : Store.Path.t option;
      limit : int option;
      order : [ `Random of Random.State.t | `Sorted | `Undefined ];
    }

    let v ?depth ?prefix ?limit ?(order = `Undefined) () =
      { depth; prefix; limit; order }

    let default = v ()
  end

  let rec combine_paths prefix k =
    match Store.Path.decons prefix with
    | Some (step, path) -> combine_paths path (Store.Path.cons step k)
    | None -> k

  let items ?(options = Options.default) store :
      (Store.path * Store.contents) Lwt_seq.t Lwt.t =
    let* prefix, tree =
      match options.prefix with
      | Some prefix ->
          let+ t = Store.get_tree store prefix in
          (prefix, t)
      | None ->
          let+ t = Store.tree store in
          (Store.Path.empty, t)
    in
    let exception Return of (Store.path * Store.contents) Lwt_seq.t in
    let count = ref 0 in
    let limit = match options.limit with Some x -> x | None -> 0 in
    let contents path c acc =
      if Option.is_some options.limit && !count >= limit then raise (Return acc)
      else
        let () = incr count in
        Lwt.return (Lwt_seq.cons (combine_paths prefix path, c) acc)
    in
    Lwt.catch
      (fun () ->
        Store.Tree.fold ~order:options.order ?depth:options.depth tree ~contents
          Lwt_seq.empty)
      (function Return acc -> Lwt.return acc | exn -> raise exn)

  let paths ?options store =
    let+ x = items ?options store in
    Lwt_seq.map fst x

  let exec (type a) (t : a t) ?options store =
    let enable_cache = enable_cache t in
    let cache = cache t in
    let f = f t in
    let* head = Store.Head.get store in
    let hash = Store.Commit.hash head in
    let cache : (Store.path, a option) Hashtbl.t =
      match Cache.Lru.find_opt cache hash with
      | Some x -> x
      | None ->
          let ht = Hashtbl.create 8 in
          let () = if enable_cache then Cache.Lru.add cache hash ht in
          ht
    in
    let inner (k, v) : a option Lwt.t =
      let* x =
        match (enable_cache, Hashtbl.find_opt cache k) with
        | true, Some x -> Lwt.return x
        | true, None ->
            let+ x = f k v in
            Hashtbl.replace cache k x;
            x
        | _ -> f k v
      in
      Lwt.return x
    in
    let+ (items : (Store.path * Store.contents) Lwt_seq.t) =
      items ?options store
    in
    Lwt_seq.filter_map_s inner items
end

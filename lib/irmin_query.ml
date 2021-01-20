open Lwt.Syntax
include Irmin_query_intf

module Make (X : Irmin.S) : QUERY with module Store = X = struct
  module Store = X

  module Filter = struct
    module Cache = struct
      type t = (Store.commit, (Store.key, bool) Hashtbl.t) Hashtbl.t
    end

    type f = Store.key -> Store.contents -> bool Lwt.t

    type t = Cache.t * f

    let v f =
      let cache = Hashtbl.create 8 in
      (cache, f)

    let cache (cache, _) = cache

    let f (_, f) = f
  end

  module Iter = struct
    module Cache = struct
      type 'a t = (Store.commit, (Store.key, 'a) Hashtbl.t) Hashtbl.t
    end

    type 'a f = Store.key -> Store.contents -> 'a Lwt.t

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
      prefix : Store.Key.t option;
      root : Store.Key.t;
      limit : int option;
    }

    let default =
      { depth = None; prefix = None; root = Store.Key.empty; limit = None }
  end

  module Results = struct
    type 'a t = 'a Seq.t

    let rec iter f = function
      | Seq.Nil -> Lwt.return ()
      | Seq.Cons (x, seq) ->
          let* () = f x in
          iter f (seq ())

    let iter f stream = iter f (stream ())

    let rec map f = function
      | Seq.Nil -> Lwt.return Seq.Nil
      | Seq.Cons (x, seq) ->
          let* x = f x in
          let+ i = map f (seq ()) in
          Seq.Cons (x, fun () -> i)

    let map f stream =
      let+ i = map f (stream ()) in
      fun () -> i

    let rec fold f stream x =
      match stream () with
      | Seq.Nil -> Lwt.return x
      | Seq.Cons (x', seq) ->
          let* x = f x' x in
          fold f seq x

    let rec count stream =
      match stream () with Seq.Nil -> 0 | Seq.Cons (_, seq) -> 1 + count seq
  end

  let rec combine_keys prefix k =
    match Store.Key.decons prefix with
    | Some (step, key) -> combine_keys key (Store.Key.cons step k)
    | None -> k

  let keys ?(settings = Settings.default) store : Store.key Seq.t Lwt.t =
    let* prefix, tree =
      match settings.prefix with
      | Some prefix ->
          let+ t = Store.get_tree store prefix in
          (prefix, t)
      | None ->
          let+ t = Store.tree store in
          (Store.Key.empty, t)
    in
    let contents key _ acc =
      Lwt.return (Seq.cons (combine_keys prefix key) acc)
    in
    Store.Tree.fold ?depth:settings.depth tree ~contents Seq.empty

  let items ?(settings = Settings.default) store :
      (Store.key * Store.contents) Seq.t Lwt.t =
    let* prefix, tree =
      match settings.prefix with
      | Some prefix ->
          let+ t = Store.get_tree store prefix in
          (prefix, t)
      | None ->
          let+ t = Store.tree store in
          (Store.Key.empty, t)
    in
    let contents key c acc =
      Lwt.return (Seq.cons (combine_keys prefix key, c) acc)
    in
    Store.Tree.fold ?depth:settings.depth tree ~contents Seq.empty

  let iter' ?settings f store results : 'a Seq.t Lwt.t =
    let settings =
      match settings with Some x -> x | None -> Settings.default
    in
    let limit = match settings.limit with Some x -> x | None -> 0 in
    let pure = Iter.pure f in
    let cache = Iter.cache f in
    let f = Iter.f f in
    let* head = Store.Head.get store in
    let cache : (Store.key, 'a) Hashtbl.t =
      match Hashtbl.find_opt cache head with
      | Some x -> x
      | None ->
          let ht = Hashtbl.create 8 in
          Hashtbl.replace cache head ht;
          ht
    in
    let rec inner seq count =
      match seq with
      | Seq.Nil -> Lwt.return Seq.Nil
      | Seq.Cons ((k, v), seq) ->
          if limit > 0 && count >= limit then Lwt.return Seq.Nil
          else
            let* s = inner (seq ()) (count + 1) in
            let* x =
              match (pure, Hashtbl.find_opt cache k) with
              | true, Some x -> Lwt.return x
              | true, None ->
                  let+ x = f k v in
                  Hashtbl.replace cache k x;
                  x
              | _ -> f k v
            in
            Lwt.return @@ Seq.Cons (x, fun () -> s)
    in
    let+ x = inner (results ()) 0 in
    fun () -> x

  let iter (f : 'a Iter.t) ?settings store =
    let* items = items ?settings store in
    iter' ?settings f store items

  let filter :
      filter:Filter.t ->
      'a Iter.t ->
      ?settings:Settings.t ->
      Store.t ->
      'a Seq.t Lwt.t =
   fun ~filter f ?settings store ->
    let* head = Store.Head.get store in
    let cache = Filter.cache filter in
    let filter = Filter.f filter in
    let filter_cache : (Store.key, bool) Hashtbl.t =
      match Hashtbl.find_opt cache head with
      | Some x -> x
      | None ->
          let ht = Hashtbl.create 8 in
          Hashtbl.replace cache head ht;
          ht
    in
    let* items = items ?settings store in
    let rec inner seq =
      match seq () with
      | Seq.Nil -> Lwt.return Seq.Nil
      | Seq.Cons ((k, v), seq) ->
          let* (ok : bool) =
            match Hashtbl.find_opt filter_cache k with
            | Some ok -> Lwt.return ok
            | None ->
                let+ ok = filter k v in
                let () = Hashtbl.replace filter_cache k ok in
                ok
          in
          let* i = inner seq in
          if ok then Lwt.return @@ Seq.Cons ((k, v), fun () -> i) else inner seq
    in
    let* x = inner items in
    iter' ?settings f store (fun () -> x)
end

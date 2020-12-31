open Lwt.Syntax

module type QUERY = sig
  module Store : Irmin.S

  module Results : sig
    type 'a t

    val iter : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

    val fold : ('a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t
  end

  module Settings : sig
    type t = {
      depth : int option;
      prefix : Store.Key.t option;
      initial_key : Store.Key.t;
    }

    val default : t
  end

  module Filter : sig
    type f = Store.key -> (unit -> Store.contents Lwt.t) -> bool Lwt.t

    type t

    val v : f -> t

    val f : t -> f
  end

  module Iter : sig
    type 'a f = Store.key -> Store.contents -> 'a Lwt.t

    type 'a t

    val v : ?pure:bool -> 'a f -> 'a t

    val f : 'a t -> 'a f
  end

  type lazy_value = unit -> Store.Contents.t Lwt.t

  val keys : ?settings:Settings.t -> Store.t -> Store.Key.t Seq.t Lwt.t

  val iter : 'a Iter.t -> ?settings:Settings.t -> Store.t -> 'a Results.t Lwt.t

  val filter :
    filter:Filter.t ->
    'a Iter.t ->
    ?settings:Settings.t ->
    Store.t ->
    'a Results.t Lwt.t
end

module type VALUE = sig
  type t
end

module Make (Store : Irmin.S) (Value : VALUE with type t = Store.Contents.t) :
  QUERY with module Store = Store = struct
  module Store = Store

  module Results = struct
    type 'a t = 'a Lwt_stream.t

    let iter = Lwt_stream.iter_s

    let fold = Lwt_stream.fold_s
  end

  module Filter = struct
    module Cache = struct
      type t = (Store.commit, (Store.key, bool) Hashtbl.t) Hashtbl.t
    end

    type f = Store.key -> (unit -> Store.contents Lwt.t) -> bool Lwt.t

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
      depth : int option;
      prefix : Store.Key.t option;
      initial_key : Store.Key.t;
    }

    let default = { depth = None; prefix = None; initial_key = Store.Key.empty }
  end

  type lazy_value = unit -> Store.Contents.t Lwt.t

  let find_value store key : lazy_value = fun () -> Store.get store key

  let rec key_has_prefix ~prefix key =
    match prefix with
    | Some prefix -> (
        match (Store.Key.decons prefix, Store.Key.decons key) with
        | Some (h, pr), Some (i, k) ->
            let eq = Repr.unstage (Repr.equal Store.Key.step_t) in
            eq h i && key_has_prefix ~prefix:(Some pr) k
        | Some _, None -> false
        | None, _ -> true )
    | None -> true

  let keys ?(settings = Settings.default) store : Store.key Seq.t Lwt.t =
    let d = match settings.depth with Some x -> x | None -> 0 in
    let rec inner key depth seq =
      if d > 0 && depth >= d then Lwt.return seq
      else
        let* items = Store.list store key in
        Lwt_list.fold_left_s
          (fun acc x ->
            match x with
            | step, `Contents ->
                let key' = Store.Key.rcons key step in
                if key_has_prefix ~prefix:settings.prefix key' then
                  Lwt.return (Seq.cons key' acc)
                else Lwt.return acc
            | step, `Node -> inner (Store.Key.rcons key step) (depth + 1) acc)
          seq items
    in
    inner settings.initial_key 0 Seq.empty

  let iter' f store stream =
    let pure = Iter.pure f in
    let cache = Iter.cache f in
    let f = Iter.f f in
    let+ head = Store.Head.get store in
    let cache : (Store.key, 'a) Hashtbl.t =
      match Hashtbl.find_opt cache head with
      | Some x -> x
      | None ->
          let ht = Hashtbl.create 8 in
          Hashtbl.replace cache head ht;
          ht
    in
    Lwt_stream.map_s
      (fun k ->
        match (pure, Hashtbl.find_opt cache k) with
        | true, Some x -> Lwt.return x
        | true, None ->
            let* v = find_value store k () in
            let+ x = f k v in
            Hashtbl.replace cache k x;
            x
        | _ ->
            let* v = find_value store k () in
            f k v)
      stream

  let iter (f : 'a Iter.t) ?settings store =
    let* keys = keys ?settings store in
    let stream = Lwt_stream.of_seq keys in
    iter' f store stream

  let filter :
      filter:Filter.t ->
      'a Iter.t ->
      ?settings:Settings.t ->
      Store.t ->
      'a Results.t Lwt.t =
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
    let* keys = keys ?settings store in
    let stream = Lwt_stream.of_seq keys in
    Lwt_stream.filter_s
      (fun k ->
        let v = find_value store k in
        let+ (ok : bool) =
          match Hashtbl.find_opt filter_cache k with
          | Some ok -> Lwt.return ok
          | None ->
              let+ ok = filter k v in
              let () = Hashtbl.replace filter_cache k ok in
              ok
        in
        ok)
      stream
    |> iter' f store
end

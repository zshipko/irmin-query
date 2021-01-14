open Lwt.Syntax

module type QUERY = sig
  module Store : Irmin.S

  module Settings : sig
    type t = {
      depth : int option;
      prefix : Store.Key.t option;
      root : Store.Key.t;
      limit : int option;
    }

    val default : t
  end

  module Filter : sig
    type f = Store.key -> Store.contents Lwt.t lazy_t -> bool Lwt.t

    type t

    val v : f -> t

    val f : t -> f
  end

  module Iter : sig
    type 'a f = Store.key -> Store.contents Lwt.t lazy_t -> 'a Lwt.t

    type 'a t

    val v : ?pure:bool -> 'a f -> 'a t

    val f : 'a t -> 'a f
  end

  module Results : sig
    type 'a t = 'a Seq.t

    val iter : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

    val map : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t

    val fold : ('a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

    val count : 'a t -> int
  end

  val keys : ?settings:Settings.t -> Store.t -> Store.Key.t Results.t Lwt.t

  val iter : 'a Iter.t -> ?settings:Settings.t -> Store.t -> 'a Results.t Lwt.t

  val filter :
    filter:Filter.t ->
    'a Iter.t ->
    ?settings:Settings.t ->
    Store.t ->
    'a Results.t Lwt.t
end

module Make (X : Irmin.S) : QUERY with module Store = X = struct
  module Store = X

  module Filter = struct
    module Cache = struct
      type t = (Store.commit, (Store.key, bool) Hashtbl.t) Hashtbl.t
    end

    type f = Store.key -> Store.contents Lwt.t lazy_t -> bool Lwt.t

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

    type 'a f = Store.key -> Store.contents Lwt.t lazy_t -> 'a Lwt.t

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

  let find_value store key = Lazy.from_fun (fun () -> Store.get store key)

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
    inner settings.root 0 Seq.empty

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
      | Seq.Cons (k, seq) ->
          if limit > 0 && count >= limit then Lwt.return Seq.Nil
          else
            let* s = inner (seq ()) (count + 1) in
            let* x =
              match (pure, Hashtbl.find_opt cache k) with
              | true, Some x -> Lwt.return x
              | true, None ->
                  let v = find_value store k in
                  let+ x = f k v in
                  Hashtbl.replace cache k x;
                  x
              | _ ->
                  let v = find_value store k in
                  f k v
            in
            Lwt.return @@ Seq.Cons (x, fun () -> s)
    in
    let+ x = inner (results ()) 0 in
    fun () -> x

  let iter (f : 'a Iter.t) ?settings store =
    let* keys = keys ?settings store in
    iter' ?settings f store keys

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
    let* keys = keys ?settings store in
    let rec inner seq =
      match seq () with
      | Seq.Nil -> Lwt.return Seq.Nil
      | Seq.Cons (k, seq) ->
          let v = find_value store k in
          let* (ok : bool) =
            match Hashtbl.find_opt filter_cache k with
            | Some ok -> Lwt.return ok
            | None ->
                let+ ok = filter k v in
                let () = Hashtbl.replace filter_cache k ok in
                ok
          in
          let* i = inner seq in
          if ok then Lwt.return @@ Seq.Cons (k, fun () -> i) else inner seq
    in
    let* x = inner keys in
    iter' ?settings f store (fun () -> x)
end

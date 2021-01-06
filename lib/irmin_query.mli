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
    type f = Store.key -> Store.Contents.t Lwt.t lazy_t -> bool Lwt.t

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

  val keys : ?settings:Settings.t -> Store.t -> Store.Key.t Seq.t Lwt.t

  val iter : 'a Iter.t -> ?settings:Settings.t -> Store.t -> 'a Seq.t Lwt.t

  val filter :
    filter:Filter.t ->
    'a Iter.t ->
    ?settings:Settings.t ->
    Store.t ->
    'a Seq.t Lwt.t
end

module Make (Store : Irmin.S) : QUERY with module Store = Store

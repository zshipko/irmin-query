module type QUERY = sig
  module Store : Irmin.S

  type lazy_value = unit -> Store.Contents.t Lwt.t

  module Results : sig
    type 'a t

    val iter : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

    val map : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t

    val fold : ('a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

    val to_seq : 'a t -> 'a Seq.t
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
    type f = Store.key -> lazy_value -> bool Lwt.t

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
  QUERY with module Store = Store

module type S = sig
  module Store : Irmin.S

  module Settings : sig
    type t = {
      depth : Store.Tree.depth option;
      prefix : Store.Path.t option;
      limit : int option;
      order : [ `Random of Random.State.t | `Sorted | `Undefined ];
    }

    val default : t
  end

  module Filter : sig
    type f = Store.path -> Store.contents -> bool Lwt.t
    type t

    val v : ?pure:bool -> f -> t
    val f : t -> f
  end

  module Iter : sig
    type 'a f = Store.path -> Store.contents -> 'a Lwt.t
    type 'a t

    val v : ?pure:bool -> 'a f -> 'a t
    val f : 'a t -> 'a f
  end

  module Results = Lwt_seq

  val paths : ?settings:Settings.t -> Store.t -> Store.path Results.t Lwt.t

  val items :
    ?settings:Settings.t ->
    Store.t ->
    (Store.path * Store.contents) Results.t Lwt.t

  val map : 'a Iter.t -> ?settings:Settings.t -> Store.t -> 'a Results.t Lwt.t

  val filter_map :
    filter:Filter.t ->
    'a Iter.t ->
    ?settings:Settings.t ->
    Store.t ->
    'a Results.t Lwt.t

  val reduce : ('a -> 'b -> 'b Lwt.t) -> 'a Results.t -> 'b -> 'b Lwt.t
end

module type Irmin_query = sig
  module type S = S

  module Make : functor (X : Irmin.S) -> S with module Store = X
end

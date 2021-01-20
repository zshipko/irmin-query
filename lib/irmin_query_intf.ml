module type QUERY = sig
  module Store : Irmin.S

  module Settings : sig
    type t = {
      depth : Store.Tree.depth option;
      prefix : Store.Key.t option;
      root : Store.Key.t;
      limit : int option;
    }

    val default : t
  end

  module Filter : sig
    type f = Store.key -> Store.contents -> bool Lwt.t

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

  module Results : sig
    type 'a t = 'a Seq.t

    val iter : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

    val map : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t

    val fold : ('a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

    val count : 'a t -> int
  end

  val keys : ?settings:Settings.t -> Store.t -> Store.key Results.t Lwt.t

  val items :
    ?settings:Settings.t ->
    Store.t ->
    (Store.key * Store.contents) Results.t Lwt.t

  val iter : 'a Iter.t -> ?settings:Settings.t -> Store.t -> 'a Results.t Lwt.t

  val filter :
    filter:Filter.t ->
    'a Iter.t ->
    ?settings:Settings.t ->
    Store.t ->
    'a Results.t Lwt.t
end

module type Irmin_query = sig
  module type QUERY = QUERY

  module Make : functor (X : Irmin.S) -> QUERY with module Store = X
end

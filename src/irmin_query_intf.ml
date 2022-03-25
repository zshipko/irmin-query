module type S = sig
  module Store : Irmin.S

  module Options : sig
    type t = {
      depth : Store.Tree.depth option;
      prefix : Store.Path.t option;
      limit : int option;
      order : [ `Random of Random.State.t | `Sorted | `Undefined ];
    }

    val default : t
  end

  type 'a f = Store.path -> Store.contents -> 'a option Lwt.t
  type 'a t

  val v : ?pure:bool -> 'a f -> 'a t
  val f : 'a t -> 'a f

  module Results = Lwt_seq

  val paths : ?options:Options.t -> Store.t -> Store.path Results.t Lwt.t

  val items :
    ?options:Options.t ->
    Store.t ->
    (Store.path * Store.contents) Results.t Lwt.t

  val exec : 'a t -> ?options:Options.t -> Store.t -> 'a Results.t Lwt.t
  val fold : ('a -> 'b -> 'b Lwt.t) -> 'a Results.t -> 'b -> 'b Lwt.t
end

module type Irmin_query = sig
  module type S = S

  module Make : functor (X : Irmin.S) -> S with module Store = X
end

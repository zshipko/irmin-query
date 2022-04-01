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

  val v : ?cache:bool -> 'a f -> 'a t
  val reset : 'a t -> unit

  val items :
    ?options:Options.t ->
    Store.t ->
    (Store.path * Store.contents) Lwt_seq.t Lwt.t

  val paths : ?options:Options.t -> Store.t -> Store.path Lwt_seq.t Lwt.t
  val exec : 'a t -> ?options:Options.t -> Store.t -> 'a Lwt_seq.t Lwt.t
end

module type Irmin_query = sig
  module type S = S

  module Make : functor (X : Irmin.S) -> S with module Store = X
end

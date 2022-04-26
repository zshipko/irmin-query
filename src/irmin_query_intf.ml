module type S = sig
  module Store : Irmin.S

  type 'a with_options =
    ?depth:Store.Tree.depth ->
    ?prefix:Store.Path.t ->
    ?limit:int ->
    ?order:[ `Random of Random.State.t | `Sorted | `Undefined ] ->
    'a

  type 'a f = Store.path -> Store.contents -> 'a option Lwt.t
  type 'a t

  val v : ?cache:bool -> 'a f -> 'a t
  val reset : 'a t -> unit

  val contents :
    (Store.t -> (Store.path * Store.contents) Lwt_seq.t Lwt.t) with_options

  val tree : (Store.t -> (Store.path * Store.tree) Lwt_seq.t Lwt.t) with_options
  val list : (Store.t -> Store.path Lwt_seq.t Lwt.t) with_options
  val exec : ('a t -> Store.t -> 'a Lwt_seq.t Lwt.t) with_options
end

module type Irmin_query = sig
  module type S = S

  module Make : functor (X : Irmin.S) -> S with module Store = X
end

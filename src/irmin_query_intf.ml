module type S = sig
  module Store : Irmin.Generic_key.S

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

  module Expr : sig
    type 'a t

    val find : Store.path -> Store.contents option t
    val get : Store.path -> Store.contents t
    val find_tree : Store.path -> Store.tree option t
    val get_tree : Store.path -> Store.tree t
    val remove : Store.path -> unit t
    val set : Store.path -> Store.contents t -> unit t
    val set_tree : Store.path -> Store.tree t -> unit t
    val value : 'a -> 'a t
    val map : ('a -> 'b Lwt.t) -> 'a t -> 'b t
    val ( let& ) : 'a t -> ('a -> 'b Lwt.t) -> 'b t

    val exec :
      ?parents:Store.commit list ->
      ?path:Store.path ->
      info:Store.Info.f ->
      Store.t ->
      'a t ->
      'a Lwt.t
  end
end

module type Irmin_query = sig
  module type S = S

  module Make : functor (X : Irmin.Generic_key.S) -> S with module Store = X
end

module type S = sig
  module Store : Irmin.Generic_key.S

  type 'a with_options =
    ?depth:Store.Tree.depth ->
    ?prefix:Store.Path.t ->
    ?limit:int ->
    ?order:[ `Random of Random.State.t | `Sorted | `Undefined ] ->
    'a

  val contents :
    (Store.t -> (Store.path * Store.contents) Lwt_seq.t Lwt.t) with_options

  val trees :
    (Store.t -> (Store.path * Store.tree) Lwt_seq.t Lwt.t) with_options

  val nodes :
    (Store.t -> (Store.path * Store.node) Lwt_seq.t Lwt.t) with_options

  val keys : (Store.t -> Store.path Lwt_seq.t Lwt.t) with_options

  module Cache : sig
    type 'a t

    val create : int -> 'a t
  end

  val select :
    (?cache:'a option Cache.t ->
    (Store.path -> Store.contents -> 'a option Lwt.t) ->
    Store.t ->
    'a Lwt_seq.t Lwt.t)
    with_options

  val update :
    (?parents:Store.Commit.t list ->
    ?strategy:[ `Set | `Merge | `Test_and_set ] ->
    info:Store.Info.f ->
    (Store.path -> Store.contents -> Store.contents option Lwt.t) ->
    Store.t ->
    unit Lwt.t)
    with_options

  module Expr : sig
    type 'a t

    val path : Store.path -> Store.path t
    val find : Store.path t -> Store.contents option t
    val get : Store.path t -> Store.contents t
    val find_tree : Store.path t -> Store.tree option t
    val get_tree : Store.path t -> Store.tree t
    val remove : Store.path t -> unit t
    val set : Store.path t -> Store.contents t -> unit t
    val set_tree : Store.path t -> Store.tree t -> unit t
    val value : 'a -> 'a t
    val bind : ('a -> 'b t Lwt.t) -> 'a t -> 'b t
    val map : ('a -> 'b Lwt.t) -> 'a t -> 'b t
    val list : Store.tree t -> (Store.step * Store.tree) list t
    val join : 'a t -> 'b t -> 'b t
    val rename : from:Store.path t -> Store.Path.t t -> unit t
    val ( let& ) : 'a t -> ('a -> 'b t Lwt.t) -> 'b t
    val ( let| ) : 'a t -> ('a -> 'b Lwt.t) -> 'b t
    val ( & ) : 'a t -> 'b t -> 'b t
    val eval_tree : 'a t -> Store.tree -> (Store.tree * 'a) Lwt.t

    val eval :
      ?parents:Store.commit list ->
      ?prefix:Store.path ->
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

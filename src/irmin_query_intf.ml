module type S = sig
  module Store : Irmin.Generic_key.S

  val contents :
    ?max_depth:int ->
    Store.tree ->
    (Store.path * Store.contents) Lwt_seq.t Lwt.t

  val trees :
    ?max_depth:int -> Store.tree -> (Store.path * Store.tree) Lwt_seq.t Lwt.t

  val nodes :
    ?max_depth:int -> Store.tree -> (Store.path * Store.node) Lwt_seq.t Lwt.t

  val paths : ?max_depth:int -> Store.tree -> Store.path Lwt_seq.t Lwt.t

  module Cache : sig
    type 'a t

    val create : int -> 'a t
    val clear : 'a t -> unit
  end

  val select :
    ?limit:int ->
    ?max_depth:int ->
    ?cache:'a option Cache.t ->
    (Store.path -> Store.contents -> 'a option Lwt.t) ->
    Store.t ->
    Store.Path.t ->
    'a Lwt_seq.t Lwt.t

  val update :
    ?max_depth:int ->
    ?prefix:Store.Path.t ->
    ?parents:Store.Commit.t list ->
    ?strategy:[ `Set | `Merge | `Test_and_set ] ->
    info:Store.Info.f ->
    (Store.path -> Store.contents -> Store.contents option Lwt.t) ->
    Store.t ->
    Store.Path.t ->
    unit Lwt.t

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

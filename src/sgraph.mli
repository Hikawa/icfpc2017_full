module type S = sig
  type t
  module V: sig
    type t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool
    val t_of_sexp: Core.Sexp.t -> t
    val sexp_of_t: t -> Core.Sexp.t
  end

  module E: sig
    type t
    type label
    val label: t -> label
      (*
  val t_of_sexp: Core.Sexp.t -> t
  val sexp_of_t: t -> Core.Sexp.t
*)
  end

  val iter_vertex : (V.t -> unit) -> t -> unit
  (*val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a*)
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  (*val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
    val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a
    val nb_vertex : t -> int*)
end

open Mir

val pp_indexed : 'a Fmt.t -> Format.formatter -> string * 'a index list -> unit
val pp_expr_typed_located : Format.formatter -> mtype_loc_ad with_expr -> unit

val pp_typed_prog :
  Format.formatter -> ('a with_expr, ('b, 'c) stmt_with) prog -> unit

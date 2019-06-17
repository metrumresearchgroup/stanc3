open Core_kernel
include module type of Mir
module Pretty : module type of Mir_pretty_printer
module Validation : module type of Validation
module State : module type of State
module Utils : module type of Utils

val string_of_location : location -> string
val string_of_location_span : location_span -> string
val operator_of_string : string -> Operator.t option
val string_of_operator : Operator.t -> string
val string_of_internal_fn : internal_fn -> string
val internal_fn_of_string : string -> internal_fn option

val internal_funapp :
  Mir.internal_fn -> 'a Expr.t list -> 'a -> 'a Expr.t

val no_loc : location
val no_span : location_span
val merge_spans : location_span -> location_span -> location_span
val internal_meta : Expr.Typed.meta
val loop_bottom : Expr.Typed.t
val remove_size : 'a SizedType.t -> UnsizedType.t
val zero : Mir.Expr.Typed.t
val remove_possible_size : 'a PossiblySizedType.t -> UnsizedType.t
val sexp_of_expr_typed_located : Expr.Typed.t -> Sexp.t
val sexp_of_stmt_loc : Stmt.Typed.t -> Sexp.t
val gensym : unit -> string
val gensym_enter : unit -> string * (unit -> unit)
val gensym_reset_danger_use_cautiously : unit -> unit

val check_compatible_arguments_mod_conv :
     string
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> bool
(** Check that the rhs list of function argument types can be converted to the
    lhs *)

val check_of_same_type_mod_array_conv :
  string -> UnsizedType.t -> UnsizedType.t -> bool
(** Check that the rhs type can be converted to the lhs, where we allow
    conversion inside an array constructor *)

val stan_math_returntype :
  string -> (UnsizedType.autodifftype * UnsizedType.t) list -> UnsizedType.returntype option
(** Get an optional return type for a Stan Math library function, given its name and argument types. *)

val assignmentoperator_stan_math_return_type :
     Operator.t
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> UnsizedType.returntype option

val operator_stan_math_return_type :
     Operator.t
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> UnsizedType.returntype option

val pretty_print_math_lib_operator_sigs : Operator.t -> string list

val pretty_print_math_lib_assignmentoperator_sigs :
  Operator.t -> string option

val pretty_print_all_math_lib_fn_sigs : string -> string

val is_stan_math_function_name : string -> bool
(** Check whether a string is the name of a Stan Math library function. *)

val stan_distribution_name_suffix : string -> string

val operator_return_type_from_string :
  string -> (UnsizedType.autodifftype * UnsizedType.t) sexp_list -> UnsizedType.returntype option

val operator_return_type :
  Operator.t -> (UnsizedType.autodifftype * UnsizedType.t) sexp_list -> UnsizedType.returntype option

val string_of_operators : (string, string sexp_list) Map.Poly.t
val ternary_if : string
val expr_from_idx : Expr.Typed.t IndexedExpr.t -> Expr.Typed.t sexp_list

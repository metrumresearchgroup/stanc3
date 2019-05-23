open Mir

val stan_math_returntype :
  string -> (autodifftype * unsizedtype) list -> returntype option
(** Get an optional return type for a Stan Math library function, given its name and argument types. *)

val is_stan_math_function_name : string -> bool
(** Check whether a string is the name of a Stan Math library function. *)

val list_all_math_lib_fn_sigs : string -> unsizedtype list
(** List all the signatures of a math library function, for the purposes of error messages. *)

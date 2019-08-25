open Core_kernel
open Fmt
open Middle

let rec scalar = function
  | ad, UArray t -> scalar (ad, t)
  | _, UInt -> "int"
  | DataOnly, _ -> "double"
  | AutoDiffable, _ -> "var"

let pp_unsized ppf (ad, ut) =
  Expression_gen.pp_unsizedtype_custom_scalar ppf (scalar (ad, ut), ut)

let pp_arg ppf (ad, ut) =
  let ref_str = match ut with UInt -> "" | _ -> "&" in
  pf ppf "const %a%s" pp_unsized (ad, ut) ref_str

let promoted_adtype =
  List.fold
    ~f:(fun accum e -> match e with AutoDiffable -> AutoDiffable | _ -> accum)
    ~init:DataOnly

let pp_rt ppf (ad, rt) =
  match rt with
  | Void -> string ppf "void"
  | ReturnType ut -> pp_unsized ppf (ad, ut)

(* XXX - also need doubles for scalar values. Always doing var now. *)

let pp_sigs_for_fn name ppf (rt, args) =
  pf ppf "@[<hov>template@ %a@ %s(@[<hov>%a@]);@]@,@," pp_rt
    (promoted_adtype (List.map ~f:fst args), rt)
    name (list ~sep:comma pp_arg) args

type arg = autodifftype * unsizedtype [@@deriving compare, sexp]

(* Ways to slim down the number of instantiations to compile:
1. Don't compile deprecated names for e.g. _log - only _lpdf
2. "eigen_vector_types" typically has both Vector and RowVector +
   UArray versions of each. This can be just UArray Vector.
3. "all_vector_types" [int+real x (scal+array) + row/vec] -> just Vector
4. Vectorized unary functions can just do 8 levels deep of one thing, not 4
*)

let problem_fn_prefixes =
  [ "sort" (* takes by value *); "logical" (* missing & *); "lbeta"
  (* missing & *) ]

(* Other issues:
   1. Should logical_gte be passed by value instead of reference for vars?
   2. Ah shit, some of these are Stan functions, not Stan Math. Decompile them?

*)

let pp_sigs ppf sigs =
  Hashtbl.iteri sigs ~f:(fun ~key ~data ->
      if
        not
          (List.exists problem_fn_prefixes ~f:(fun prefix ->
               String.is_prefix ~prefix key ))
      then pf ppf "@[<hov>%a@]@," (list ~sep:cut (pp_sigs_for_fn key)) data )

let pp ppf sigs =
  pf ppf {|
#include <stan/math.hpp>
namespace stan { namespace math {
%s
}}
|}
    (strf "%a" pp_sigs sigs)

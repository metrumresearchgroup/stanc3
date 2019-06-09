(** The Middle Intermediate Representation, which program transformations
    operate on *)

open Core_kernel
open State.Cps

(** Source code locations *)
type location =
  { filename: string
  ; line_num: int
  ; col_num: int
  ; included_from: location option }
[@@deriving sexp]

(** Delimited locations *)
type location_span = {begin_loc: location; end_loc: location} [@@deriving sexp]

(** Arithmetic and logical operators *)
type operator =
  | Plus
  | PPlus
  | Minus
  | PMinus
  | Times
  | Divide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Pow
  | Or
  | And
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq
  | PNot
  | Transpose
[@@deriving sexp, hash, compare]

(** Unsized types for function arguments and for decorating expressions
    during type checking; we have a separate type here for Math library
    functions as these functions can be overloaded, so do not have a unique
    type in the usual sense. Still, we want to assign a unique type to every
    expression during type checking.  *)
type unsizedtype =
  | UInt
  | UReal
  | UVector
  | URowVector
  | UMatrix
  | UArray of unsizedtype
  | UFun of (autodifftype * unsizedtype) list * returntype
  | UMathLibraryFunction
[@@deriving sexp, hash]

(** Flags for data only arguments to functions *)
and autodifftype = DataOnly | AutoDiffable [@@deriving sexp, hash, compare]

and returntype = Void | ReturnType of unsizedtype [@@deriving sexp, hash]

(** Sized types, for variable declarations *)
type 'e sizedtype =
  | SInt
  | SReal
  | SVector of 'e
  | SRowVector of 'e
  | SMatrix of 'e * 'e
  | SArray of 'e sizedtype * 'e
[@@deriving sexp, compare, map, hash, fold]

type 'e possiblysizedtype = Sized of 'e sizedtype | Unsized of unsizedtype
[@@deriving sexp, compare, map, hash, fold]

type litType = Int | Real | Str [@@deriving sexp, hash, compare]

(**  *)
type fun_kind = StanLib | CompilerInternal | UserDefined
[@@deriving compare, sexp, hash]

type 'e index =
  | All
  | Single of 'e
  (*
  | MatrixSingle of 'e
 *)
  | Upfrom of 'e
  | Downfrom of 'e
  | Between of 'e * 'e
  | MultiIndex of 'e
[@@deriving sexp, hash, map, fold]

and 'e expr =
  | Var of string
  | Lit of litType * string
  | FunApp of fun_kind * string * 'e list
  | TernaryIf of 'e * 'e * 'e
  | EAnd of 'e * 'e
  | EOr of 'e * 'e
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash, map, compare]

type fun_arg_decl = (autodifftype * string * unsizedtype) list
[@@deriving sexp, hash, map]

type 's fun_def =
  { fdrt: unsizedtype option
  ; fdname: string
  ; fdargs: fun_arg_decl
  ; fdbody: 's
  ; fdloc: location_span sexp_opaque [@compare.ignore] }
[@@deriving sexp, hash, map]

type 'e lvalue = string * 'e index list [@@deriving sexp, hash, map, fold]

type ('e, 's) statement =
  | Assignment of 'e lvalue * 'e
  | TargetPE of 'e
  | NRFunApp of fun_kind * string * 'e list
  | Break
  | Continue
  | Return of 'e option
  | Skip
  | IfElse of 'e * 's * 's option
  | While of 'e * 's
  (* XXX Collapse with For? *)
  | For of {loopvar: string; lower: 'e; upper: 'e; body: 's}
  (* A Block for now corresponds tightly with a C++ block:
     variables declared within it have local scope and are garbage collected
     when the block ends.*)
  | Block of 's list
  (* SList has no semantics, just programming convenience *)
  | SList of 's list
  | Decl of
      { decl_adtype: autodifftype
      ; decl_id: string
      ; decl_type: 'e possiblysizedtype }
[@@deriving sexp, hash, map, fold]

type io_block =
  | Data
  | Parameters
  | TransformedParameters
  | GeneratedQuantities
[@@deriving sexp, hash]

type 'e io_var = string * ('e sizedtype * io_block) [@@deriving sexp, map]

type ('e, 's) prog =
  { functions_block: 's fun_def list
  ; input_vars: 'e io_var list
  ; prepare_data: 's list (* data & transformed data decls and statements *)
  ; log_prob: 's list (*assumes data & params are in scope and ready*)
  ; generate_quantities: 's list (* assumes data & params ready & in scope*)
  ; transform_inits: 's list
  ; output_vars: 'e io_var list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, map]



(* == Traversals ===============================================================
  A `traversal` is like a `map` except that it also allows for effects.

  In the following functions the effect is always from `State` since this is
  what we need in dataflow analysis.
*)
let traverse_index ~f = function
  | Single e -> State.map (f e) ~f:(fun e' -> Single e')
  | Upfrom e -> State.map (f e) ~f:(fun e' -> Upfrom e')
  | Downfrom e -> State.map (f e) ~f:(fun e' -> Downfrom e')
  | MultiIndex e -> State.map (f e) ~f:(fun e' -> Upfrom e')
  | Between (lower_e, upper_e) ->
      State.(
        f lower_e
        >>= fun lower_e' ->
        f upper_e |> map ~f:(fun upper_e' -> Between (lower_e', upper_e')))
  | All -> State.return All

let traverse_expr ~f = function
  | FunApp (fn_kind, name, exprs) ->
      State.(
        List.map ~f exprs |> all
        |> map ~f:(fun exprs' -> FunApp (fn_kind, name, exprs')))
  | EAnd (le, re) ->
      State.(f le >>= fun le' -> f re |> map ~f:(fun re' -> EAnd (le', re')))
  | EOr (le, re) ->
      State.(f le >>= fun le' -> f re |> map ~f:(fun re' -> EOr (le', re')))
  | Indexed (e, idxs) ->
      State.(
        f e
        >>= fun e' ->
        List.map ~f:(traverse_index ~f) idxs
        |> all
        |> map ~f:(fun idxs' -> Indexed (e', idxs')))
  | TernaryIf (pred, te, fe) ->
      State.(
        f pred
        >>= fun pred' ->
        f te
        >>= fun te' -> f fe |> map ~f:(fun fe' -> TernaryIf (pred', te', fe')))
  | Var name -> State.return @@ Var name
  | Lit (ty, name) -> State.return @@ Lit (ty, name)



let traverse_lvalue ~f (name, idxs) =
  idxs
  |> List.map ~f:(traverse_index ~f)
  |> State.all
  |> State.map ~f:(fun idxs' -> (name, idxs'))

let rec traverse_sizedtype ~f = function
  | SVector e -> f e |> State.map ~f:(fun e' -> SVector e')
  | SRowVector e -> f e |> State.map ~f:(fun e' -> SRowVector e')
  | SArray (sized_ty, e) ->
      State.(
        traverse_sizedtype ~f sized_ty
        >>= fun sized_ty' -> f e |> map ~f:(fun e' -> SArray (sized_ty', e')))
  | SReal -> State.return SReal
  | SInt -> State.return SInt
  | SMatrix (e1, e2) ->
      State.(
        f e1 >>= fun e1' -> f e2 |> map ~f:(fun e2' -> SMatrix (e1', e2')))

let traverse_possiblysizedtype ~f = function
  | Sized sizedtype -> State.map ~f:(fun ty -> Sized ty) @@ f sizedtype
  | Unsized unsizedtype -> State.return @@ Unsized unsizedtype

(** Statement traveral with effects fixed at `State` *)
let traverse_statement ~e ~f = function
  | IfElse (pred, s_true, Some s_false) ->
      State.(
        e pred
        >>= fun pred' ->
        f s_true
        >>= fun s_true' ->
        f s_false
        |> map ~f:(fun s_false' -> IfElse (pred', s_true', Some s_false')))
  | IfElse (pred, s_true, None) ->
      State.(
        e pred
        >>= fun pred' ->
        f s_true |> map ~f:(fun s_true' -> IfElse (pred', s_true', None)))
  | While (pred, body) ->
      State.(
        e pred
        >>= fun pred' -> f body |> map ~f:(fun body' -> While (pred', body')))
  | For {loopvar; lower; upper; body} ->
      State.(
        e lower
        >>= fun lower' ->
        e upper
        >>= fun upper' ->
        f body
        |> map ~f:(fun body' ->
               For {loopvar; lower= lower'; upper= upper'; body= body'} ))
  | Block xs -> State.(List.map ~f xs |> all |> map ~f:(fun xs' -> Block xs'))
  | SList xs -> State.(List.map ~f xs |> all |> map ~f:(fun xs' -> SList xs'))
  | Assignment (lvalue, expr) ->
      State.(
        traverse_lvalue ~f:e lvalue
        >>= fun lvalue' ->
        e expr |> map ~f:(fun expr' -> Assignment (lvalue', expr')))
  | TargetPE expr -> e expr |> State.map ~f:(fun expr' -> TargetPE expr')
  | NRFunApp (fun_kind, name, exprs) ->
      exprs |> List.map ~f:e |> State.all
      |> State.map ~f:(fun exprs' -> NRFunApp (fun_kind, name, exprs'))
  | Decl decl ->
      decl.decl_type
      |> traverse_possiblysizedtype ~f:(traverse_sizedtype ~f:e)
      |> State.map ~f:(fun decl_type' -> Decl {decl with decl_type= decl_type'})
  | Return (Some expr) ->
      e expr |> State.map ~f:(fun expr' -> Return (Some expr'))
  | Return None -> State.return @@ Return None
  | Break -> State.return Break
  | Continue -> State.return Continue
  | Skip -> State.return Skip

(* == Fixed types =========================================================== *)

(* -- Expressions ----------------------------------------------------------- *)

(** Fixed-point of expressions with meta-data *)
type 'm with_expr = {expr: 'm with_expr expr; emeta: 'm}
[@@deriving compare, sexp, hash]

(** Map a `with_expr` changing the type of meta-data *)
let rec map_expr_with ~f {expr; emeta} =
  {expr= map_expr (map_expr_with ~f) expr; emeta= f emeta}

let rec fold_expr_with ~f ~init {expr ; emeta } =
  fold_expr (fun accu -> fold_expr_with ~f ~init:accu) (f init emeta) expr

(** Traverse a `with_expr` mapping the type of meta-data with effects *)
let rec traverse_expr_with ~f {expr; emeta} =
  State.(
    f emeta
    >>= fun emeta' ->
    traverse_expr ~f:(traverse_expr_with ~f) expr
    >>= fun expr' -> return @@ {expr= expr'; emeta= emeta'})

(* -- Statements ------------------------------------------------------------ *)

(** Fixed-point of statements with meta-data *)
type ('e, 'm) stmt_with =
  {stmt: ('e with_expr, ('e, 'm) stmt_with) statement; smeta: 'm}
[@@deriving sexp]

(** Map a `stmt_with` changing the type of meta-data for the statments and 
    expressions 
*)
let rec map_stmt_with ~e ~f {stmt; smeta} =
  { stmt= map_statement (map_expr_with ~f:e) (map_stmt_with ~e ~f) stmt
  ; smeta= f smeta }

let rec fold_stmt_with ~e ~f ~init {stmt;smeta} =
  fold_statement      
    (fun accu -> fold_expr_with ~f:e ~init:accu)
    (fun accu -> fold_stmt_with ~e ~f ~init:accu) 
    (f init smeta)
    stmt

let rec traverse_stmt_with ~e ~f {stmt; smeta} =
  State.(
    f smeta
    >>= fun smeta' ->
    traverse_statement stmt ~e:(traverse_expr_with ~f:e)
      ~f:(traverse_stmt_with ~e ~f)
    |> map ~f:(fun stmt' -> {stmt= stmt'; smeta= smeta'}))

type ('e, 'm) stmt_with_num = {stmtn: ('e with_expr, int) statement; smetan: 'm}
[@@deriving sexp, hash]

(* == Fixed types specialised with meta-data ================================ *)

type expr_no_meta = unit with_expr
type stmt_no_meta = (expr_no_meta, unit) stmt_with

(** Type information added during semantic check 
*)
type mtype_loc_ad =
  { mtype: unsizedtype
  ; mloc: location_span sexp_opaque [@compare.ignore]
  ; madlevel: autodifftype }
[@@deriving compare, sexp, hash]

(** Typed expressions with location info 
*)
type expr_typed_located = mtype_loc_ad with_expr
[@@deriving sexp, compare, hash]

(** Statements with typed expressions and location info 
*)
type stmt_loc =
  (mtype_loc_ad, (location_span sexp_opaque[@compare.ignore])) stmt_with
[@@deriving sexp]

(** Statements with typed expressions, location info and labels *)
type 'lbl stmt_labelled = (mtype_loc_ad, 'lbl stmt_label) stmt_with
[@@deriving sexp]

and 'lbl stmt_label =
  {location: location_span sexp_opaque [@compare.ignore]; label: 'lbl}
[@@deriving sexp]



type stmt_loc_num =
  (mtype_loc_ad, (location_span sexp_opaque[@compare.ignore])) stmt_with_num
[@@deriving sexp]

type typed_prog = (mtype_loc_ad with_expr, stmt_loc) prog [@@deriving sexp]

type internal_fn =
  | FnLength
  | FnMakeArray
  | FnMakeRowVec
  | FnNegInf
  | FnReadData
  | FnReadParam
  | FnWriteParam
  | FnConstrain
  | FnUnconstrain
  | FnCheck
  | FnPrint
  | FnReject
[@@deriving sexp]

(**  A custom comparator which ignores locations on expressions *)
module ExprComparator = struct
  type t = expr_typed_located [@@deriving sexp, compare]
end

(**  A module for sets of expressions which ignore their locations *)
module ExprSet = Set.Make (ExprComparator)

(**  A module for maps of expressions which ignore their locations *)
module ExprMap = Map.Make (ExprComparator)

open Core_kernel
open Middle

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (UnsizedType.ReturnType ut) -> ut
  | x -> raise_s [%message "Unexpected return type " (x : UnsizedType.returntype option)]

let trans_fn_kind = function
  | Ast.StanLib -> FunKind.StanLib
  | UserDefined -> UserDefined

let rec op_to_funapp op args =
  let argtypes =
    List.map ~f:(fun x -> (x.Ast.emeta.Ast.ad_level, x.emeta.type_)) args
  in
  { Expr.expr= FunApp (StanLib, string_of_operator op, trans_exprs args)
  ; emeta=
      { Expr.Typed.mtype= operator_return_type op argtypes |> unwrap_return_exn
      ; mloc= Ast.expr_loc_lub args
      ; madlevel= Ast.expr_ad_lub args } }

and trans_expr {Ast.expr; Ast.emeta} =
  let mtype = emeta.Ast.type_
  and mloc = emeta.loc
  and madlevel = emeta.ad_level in
  match expr with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, And, rhs) ->
      { Expr.expr= EAnd (trans_expr lhs, trans_expr rhs)
      ; emeta= {Expr.Typed.madlevel; mloc; mtype} }
  | BinOp (lhs, Or, rhs) ->
      { expr= EOr (trans_expr lhs, trans_expr rhs)
      ; emeta= {Expr.Typed.madlevel; mloc; mtype} }
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs]
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e]
  | _ ->
      let expr =
        match expr with
        | Ast.TernaryIf (cond, ifb, elseb) ->
            Expr.Pattern.if_then_else 
              (trans_expr cond)
              (trans_expr ifb)
              (trans_expr elseb)
            
        | Variable {name; _} -> Var name
        | IntNumeral x -> Lit (Int, x)
        | RealNumeral x -> Lit (Real, x)
        | FunApp (fn_kind, {name; _}, args) ->
            FunApp (trans_fn_kind fn_kind, name, trans_exprs args)
        | Ast.CondDistApp ({name; _}, args) ->
            FunApp (StanLib, name, trans_exprs args)
        | GetLP | GetTarget -> Var "target"
        | ArrayExpr eles ->
            FunApp
              ( CompilerInternal
              , string_of_internal_fn FnMakeArray
              , trans_exprs eles )
        | RowVectorExpr eles ->
            FunApp
              ( CompilerInternal
              , string_of_internal_fn FnMakeRowVec
              , trans_exprs eles )
        | Indexed (lhs, indices) ->
            Indexed (trans_expr lhs, List.map ~f:trans_idx indices)
        | Paren _ | BinOp _ | PrefixOp _ | PostfixOp _ ->
            raise_s [%message "Impossible!"]
      in
      {expr; emeta= {Expr.Typed.mtype; mloc; madlevel}}

and trans_idx = function
  | Ast.All -> All
  | Ast.Upfrom e -> Upfrom (trans_expr e)
  | Ast.Downfrom e -> Downfrom (trans_expr e)
  | Ast.Between (lb, ub) -> Between (trans_expr lb, trans_expr ub)
  | Ast.Single e -> (
    match e.emeta.type_ with
    | UInt -> Single (trans_expr e)
    | UArray _ -> MultiIndex (trans_expr e)
    | _ ->
        raise_s
          [%message "Expecting int or array" (e.emeta.type_ : UnsizedType.t)] )

and trans_exprs = List.map ~f:trans_expr

let trans_sizedtype = SizedType.map trans_expr

let neg_inf =
  { Expr.expr = FunApp (StanLib, string_of_internal_fn FnNegInf, [])
  ; emeta= {Expr.Typed.mtype= UReal; mloc= no_span; madlevel= DataOnly} }

let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ast_obs t =
  let trunc cond_op (x : Ast.typed_expression) y =
    let smeta = x.Ast.emeta.loc in
    { Stmt.stmt=
        IfElse
          ( op_to_funapp cond_op [ast_obs; x]
          , {smeta; stmt= TargetPE neg_inf}
          , y ) 
    ; smeta
    }
  in
  match t with
  | Ast.NoTruncate -> []
  | TruncateUpFrom lb -> [trunc Less lb None]
  | TruncateDownFrom ub -> [trunc Greater ub None]
  | TruncateBetween (lb, ub) -> [trunc Less lb (Some (trunc Greater ub None))]

let unquote s =
  if s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.drop_suffix (String.drop_prefix s 1) 1
  else s

(* hack(sean): strings aren't real
   XXX add UString to MIR and maybe AST.
*)
let mkstring mloc s =
  {Expr.expr= Lit (Str, s); emeta= {Expr.Typed.mtype= UReal; mloc; madlevel= DataOnly}}

let trans_printables mloc (ps : Ast.typed_expression Ast.printable list) =
  List.map
    ~f:(function
      | Ast.PString s -> mkstring mloc (unquote s)
      | Ast.PExpr e -> trans_expr e)
    ps

(** [add_index expression index] returns an expression that (additionally)
    indexes into the input [expression] by [index].*)
let add_int_index e i =
  let loc = Expr.Typed.location e 
  and ty = Expr.Typed.type_of e in
  let mtype =
    Semantic_check.inferred_unsizedtype_of_indexed_exn ~loc 
       ty [(i, UInt)]
  and mir_i = trans_idx i in
  let expr =
    match e.expr with
    | Expr.Pattern.Var _ -> Expr.Pattern.Indexed (e, [mir_i])
    | Expr.Pattern.Indexed (e, indices) -> Indexed (e, indices @ [mir_i])
    | _ -> raise_s [%message "These should go away with Ryan's LHS"]
  in
  {Expr.expr; emeta= {e.emeta with mtype}}

(** [mkfor] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
let mkfor upper bodyfn iteratee smeta =
  let idx s =
    Ast.Single
      (Ast.mk_typed_expression
         ~expr:(Ast.Variable {name= s; id_loc= smeta})
         ~loc:smeta ~type_:UInt ~ad_level:DataOnly)
  in
  let loopvar, reset = gensym_enter () in
  let lower = loop_bottom in
  let stmt = Stmt.Pattern.Block [bodyfn (add_int_index iteratee (idx loopvar))] in
  reset () ;
  {Stmt.stmt= For {loopvar; lower; upper; body= {stmt; smeta}}; smeta}

(** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype].

    We can call [bodyfn] directly on scalars, make a direct For loop
    around Eigen types, or for Arrays we call mkfor but inserting a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_scalar st bodyfn var smeta =
  match st with
  | SizedType.SInt | SReal -> bodyfn var
  | SVector d | SRowVector d -> mkfor d bodyfn var smeta
  | SMatrix (d1, d2) ->
      mkfor
        { expr= FunApp (StanLib, string_of_operator Times, [d1; d2])
        ; emeta= {mtype= UInt; mloc= smeta; madlevel= DataOnly} }
        bodyfn var smeta
  | SArray (t, d) -> mkfor d (fun e -> for_scalar t bodyfn e smeta) var smeta

(** [for_eigen unsizedtype...] generates a For statement that loops
    over the eigen types in the underlying [unsizedtype]; i.e. just iterating
    overarrays and running bodyfn on any eign types found within.

    We can call [bodyfn] directly on scalars and Eigen types;
    for Arrays we call mkfor but insert a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_eigen st bodyfn var smeta =
  match st with
  | SizedType.SInt | SReal | SVector _ | SRowVector _ | SMatrix _ -> bodyfn var
  | SArray (t, d) -> mkfor d (fun e -> for_eigen t bodyfn e smeta) var smeta

(* These types signal the context for a declaration during statement translation.
   They are only interpreted by trans_decl.*)
type ioaction = ReadData | ReadParam [@@deriving sexp]
type constrainaction = Check | Constrain | Unconstrain [@@deriving sexp]

let constrainaction_fname c =
  string_of_internal_fn
    ( match c with
    | Check -> FnCheck
    | Constrain -> FnConstrain
    | Unconstrain -> FnUnconstrain )

type decl_context =
  { dread: ioaction option
  ; dconstrain: constrainaction option
  ; dadlevel: UnsizedType.autodifftype }

let rec unsizedtype_to_string = function
  | UnsizedType.UMatrix -> "matrix"
  | UVector -> "vector"
  | URowVector -> "row_vector"
  | UReal -> "scalar"
  | UInt -> "integer"
  | UArray t -> unsizedtype_to_string t
  | t ->
      raise_s
        [%message "Another place where it's weird to get " (t : UnsizedType.t)]

let constraint_to_string t (c : constrainaction) =
  match t with
  | Ast.Ordered -> "ordered"
  | PositiveOrdered -> "positive_ordered"
  | Simplex -> "simplex"
  | UnitVector -> "unit_vector"
  | CholeskyCorr -> "cholesky_factor_corr"
  | CholeskyCov -> "cholesky_factor"
  | Correlation -> "corr_matrix"
  | Covariance -> "cov_matrix"
  | Lower _ -> (
    match c with
    | Check -> "greater_or_equal"
    | Constrain | Unconstrain -> "lb" )
  | Upper _ -> (
    match c with Check -> "less_or_equal" | Constrain | Unconstrain -> "ub" )
  | LowerUpper _ -> (
    match c with
    | Check ->
        raise_s
          [%message "LowerUpper is really two other checks tied together"]
    | Constrain | Unconstrain -> "lub" )
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> (
    match c with Check -> "" | Constrain | Unconstrain -> "offset_multiplier" )
  | Identity -> ""

let constraint_forl = function
  | Ast.Identity | Offset _ | Ast.Multiplier _ | Ast.OffsetMultiplier _
   |Lower _ | Upper _ | LowerUpper _ ->
      for_scalar
  | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance ->
      for_eigen

let extract_constraint_args = function
  | Ast.Lower a | Upper a | Offset a | Multiplier a -> [a]
  | LowerUpper (a1, a2) | OffsetMultiplier (a1, a2) -> [a1; a2]
  | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance | Identity ->
      []

let rec base_type = function
  | SizedType.SArray (t, _) -> base_type t
  | SVector _ | SRowVector _ | SMatrix _ -> UnsizedType.UReal
  | x -> remove_size x

let internal_of_dread = function
  | ReadParam -> FnReadParam
  | ReadData -> FnReadData

let assign_indexed vident smeta varfn (var: 'a Expr.t) =
  let indices =
    match var.expr with Indexed (_, indices) -> indices | _ -> []
  in
  {Stmt.stmt= Assignment ((vident, indices), varfn var);  smeta}

let rec eigen_size = function
  | SizedType.SArray (t, _) -> eigen_size t
  | SMatrix (d1, d2) -> [d1; d2]
  | SRowVector dim | SVector dim -> [dim]
  | SInt | SReal -> []

let param_size transform sizedtype =
  let rec shrink_eigen f st =
    match st with
    | SizedType.SArray (t, d) -> SizedType.SArray (shrink_eigen f t, d)
    | SVector d | SMatrix (d, _) -> SVector (f d)
    | SInt | SReal | SRowVector _ ->
        raise_s
          [%message
            "Expecting SVector or SMatrix, got "
              (st : Expr.Typed.meta Expr.t SizedType.t)]
  in
  let rec shrink_eigen_mat f st =
    match st with
    | SizedType.SArray (t, d) -> SizedType.SArray (shrink_eigen_mat f t, d)
    | SMatrix (d1, d2) -> SVector (f d1 d2)
    | SInt | SReal | SRowVector _ | SVector _ ->
        raise_s
          [%message
            "Expecting SMatrix, got " (st : Expr.Typed.meta Expr.t SizedType.t)]
  in
  let int num = {Expr.expr= Lit (Int, string_of_int num); emeta= internal_meta} in
  let binop e1 binop e2 =
    { Expr.expr= FunApp (StanLib, string_of_operator binop, [e1; e2])
    ; emeta= internal_meta }
  in
  let k_choose_2 k =
    binop (binop k Times (binop k Minus (int 1))) Divide (int 2)
  in
  match transform with
  | Ast.Identity | Lower _ | Upper _
   |LowerUpper (_, _)
   |Offset _ | Multiplier _
   |OffsetMultiplier (_, _)
   |Ordered | PositiveOrdered | UnitVector ->
      sizedtype
  | Simplex -> shrink_eigen (fun d -> binop d Minus (int 1)) sizedtype
  | CholeskyCorr | Correlation -> shrink_eigen k_choose_2 sizedtype
  | CholeskyCov ->
      (* (N * (N + 1)) / 2 + (M - N) * N *)
      shrink_eigen_mat
        (fun m n -> binop (k_choose_2 n) Plus (binop (binop m Minus n) Times n))
        sizedtype
  | Covariance -> shrink_eigen (fun k -> binop k Plus (k_choose_2 k)) sizedtype

let read_decl dread decl_id transform sizedtype smeta decl_var =
  let args =
    [ mkstring smeta decl_id
    ; mkstring smeta (unsizedtype_to_string @@ Expr.Typed.type_of decl_var) ]
    @ eigen_size sizedtype
  in
  let readfname = internal_of_dread dread in
  let readfn (var: Expr.Typed.t) =
    internal_funapp readfname args {var.emeta with Expr.Typed.mtype= base_type sizedtype}
  in
  let readvar (var: Expr.Typed.t) =
    match var.expr with
    | Expr.Pattern.Indexed (_, indices) -> {var with Expr.expr= Expr.Pattern.Indexed (readfn var, indices)}
    | _ -> readfn var
  in
  let forl, st =
    match dread with
    | ReadData -> (for_scalar, sizedtype)
    | ReadParam -> (for_eigen, param_size transform sizedtype)
  in
  forl st (assign_indexed decl_id smeta readvar) decl_var smeta

let constrain_decl st dconstrain t decl_id decl_var smeta =
  let mkstring = mkstring @@ Expr.Typed.location decl_var in
  match Option.map ~f:(constraint_to_string t) dconstrain with
  | None | Some "" -> []
  | Some constraint_str ->
      let dc = Option.value_exn dconstrain in
      let fname = constrainaction_fname dc in
      let args var =
        var :: mkstring constraint_str :: extract_constraint_args t
      in
      let constrainvar var =
        {Expr.expr= FunApp (CompilerInternal, fname, args var); emeta= var.emeta}
      in
      [ (constraint_forl t) st
          (assign_indexed decl_id smeta constrainvar)
          decl_var smeta ]

let rec check_decl decl_type decl_id decl_trans smeta adlevel =
  let forl = constraint_forl decl_trans in
  let chk fn args =
    let check_id id =
      let id_str =
        { Expr.expr= Lit (Str, Fmt.strf "%a" Pretty.pp_expr_typed_located id)
        ; emeta= internal_meta }
      in
      let fname = string_of_internal_fn FnCheck in
      let stmt =
        Stmt.Pattern.NRFunApp (CompilerInternal, fname, fn :: id_str :: id :: args)
      in
      {Stmt.stmt; smeta}
    in
    let mtype = remove_size decl_type in
    forl decl_type check_id
      {expr= Var decl_id; emeta= {mtype; mloc= smeta; madlevel= adlevel}}
      smeta
  in
  let args = extract_constraint_args decl_trans in
  match decl_trans with
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier (_, _) -> []
  | LowerUpper (lb, ub) ->
      check_decl decl_type decl_id (Ast.Lower lb) smeta adlevel
      @ check_decl decl_type decl_id (Ast.Upper ub) smeta adlevel
  | _ -> [chk (mkstring smeta (constraint_to_string decl_trans Check)) args]

let trans_decl {dread; dconstrain; dadlevel} smeta sizedtype transform
    identifier initial_value =
  let decl_id = identifier.Ast.name in
  let rhs = Option.map ~f:trans_expr initial_value in
  let decl_type = trans_sizedtype sizedtype in
  let decl_var =
    { Expr.expr = Var decl_id
    ; emeta= {Expr.Typed.mtype= remove_size sizedtype; madlevel= dadlevel; mloc= smeta} }
  in
  let read_stmts =
    match (dread, rhs) with
    | Some dread, _ ->
        [read_decl dread decl_id transform decl_type smeta decl_var]
    | None, Some e -> [{Stmt.stmt= Stmt.Pattern.Assignment ((decl_id, []), e); smeta}]
    | None, None -> []
  in
  let constrain_stmts =
    match dconstrain with
    | Some Constrain | Some Unconstrain ->
        constrain_decl decl_type dconstrain transform decl_id decl_var smeta
    | _ -> []
  in
  let decl =
    { Stmt.stmt= Decl {decl_adtype= dadlevel; decl_id; decl_type= Sized decl_type}
    ; smeta }
  in
  let checks =
    match dconstrain with
    | Some Check -> check_decl decl_type decl_id transform smeta dadlevel
    | _ -> []
  in
  (decl :: read_stmts) @ constrain_stmts @ checks

let unwrap_block_or_skip = function
  | [({Stmt.stmt= Block _; _} as b)] | [({stmt= Skip; _} as b)] -> b
  | x ->
      raise_s [%message "Expecting a block or skip, not" (x : stmt_loc list)]

let rec trans_stmt (declc : decl_context) (ts : Ast.typed_statement) =
  let stmt_typed = ts.stmt and smeta = ts.smeta.loc in
  let trans_stmt = trans_stmt {declc with dread= None; dconstrain= None} in
  let trans_single_stmt s = trans_stmt s |> List.hd_exn in
  let swrap stmt = [{Stmt.stmt; smeta}] in
  let mloc = smeta in
  match stmt_typed with
  | Ast.Assignment {assign_indices; assign_rhs; assign_identifier; assign_op}
    ->
      let wrap_expr expr_typed =
        Ast.mk_typed_expression ~expr:expr_typed ~loc:smeta
          ~ad_level:assign_rhs.emeta.ad_level ~type_:assign_rhs.emeta.type_
      in
      let assignee = wrap_expr @@ Ast.Variable assign_identifier in
      let assignee =
        match assign_indices with
        | [] -> assignee
        | lst -> wrap_expr @@ Ast.Indexed (assignee, lst)
      in
      let rhs =
        match assign_op with
        | Ast.Assign | Ast.ArrowAssign -> trans_expr assign_rhs
        | Ast.OperatorAssign op -> op_to_funapp op [assignee; assign_rhs]
      in
      Stmt.Pattern.Assignment
        ((assign_identifier.name, List.map ~f:trans_idx assign_indices), rhs)
      |> swrap
  | Ast.NRFunApp (fn_kind, {name; _}, args) ->
      Stmt.Pattern.NRFunApp (trans_fn_kind fn_kind, name, trans_exprs args) |> swrap
  | Ast.IncrementLogProb e | Ast.TargetPE e -> Stmt.Pattern.TargetPE (trans_expr e) |> swrap
  | Ast.Tilde {arg; distribution; args; truncation} ->
      let suffix = stan_distribution_name_suffix distribution.name in
      let name =
        distribution.name ^ Utils.proportional_to_distribution_infix ^ suffix
      in
      let add_dist =
        Stmt.Pattern.TargetPE
          { Expr.expr = Expr.Pattern.FunApp (StanLib, name, trans_exprs (arg :: args))
          ; emeta= {Expr.Typed.mloc; madlevel= Ast.expr_ad_lub (arg :: args); mtype= UReal}
          }
      in
      truncate_dist arg truncation @ [{smeta; stmt= add_dist}]
  | Ast.Print ps ->
      Stmt.Pattern.NRFunApp
        ( CompilerInternal
        , string_of_internal_fn FnPrint
        , trans_printables smeta ps )
      |> swrap
  | Ast.Reject ps ->
      Stmt.Pattern.NRFunApp
        ( CompilerInternal
        , string_of_internal_fn FnReject
        , trans_printables smeta ps )
      |> swrap
  | Ast.IfThenElse (cond, ifb, elseb) ->
      Stmt.Pattern.IfElse
        ( trans_expr cond
        , trans_single_stmt ifb
        , Option.map ~f:trans_single_stmt elseb )
      |> swrap
  | Ast.While (cond, body) ->
      Stmt.Pattern.While (trans_expr cond, trans_single_stmt body) |> swrap
  | Ast.For {loop_variable; lower_bound; upper_bound; loop_body} ->
      let body =
        match trans_single_stmt loop_body with
        | {Stmt.stmt= Block _; _} as b -> b
        | x -> {x with stmt= Block [x]}
      in
      Stmt.Pattern.For
        { loopvar= loop_variable.Ast.name
        ; lower= trans_expr lower_bound
        ; upper= trans_expr upper_bound
        ; body }
      |> swrap
  | Ast.ForEach (loopvar, iteratee, body) ->
      let newsym = gensym () in
      let wrap expr = {Expr.expr; emeta= {Expr.Typed.mloc; mtype= UInt; madlevel= DataOnly}} in
      let iteratee = trans_expr iteratee
      and indexing_var = wrap (Var newsym) in
      let assign_loopvar =
        Stmt.Pattern.Assignment
          ( (loopvar.name, [])
          , Indexed (iteratee, [Single indexing_var]) |> wrap )
      in
      let assign_loopvar = {Stmt.stmt= assign_loopvar; smeta} in
      let body =
        match trans_single_stmt body with
        | {Stmt.stmt= Block body_stmts; smeta} ->
            {Stmt.stmt= Block (assign_loopvar :: body_stmts); smeta}
        | {stmt; smeta} -> {stmt= Block [assign_loopvar; {stmt; smeta}]; smeta}
      in
      Stmt.Pattern.For
        { loopvar= newsym
        ; lower= loop_bottom
        ; upper=
            wrap @@ FunApp (StanLib, string_of_internal_fn FnLength, [iteratee])
        ; body }
      |> swrap
  | Ast.FunDef _ ->
      raise_s
        [%message
          "Found function definition statement outside of function block"]
  | Ast.VarDecl
      {sizedtype; transformation; identifier; initial_value; is_global} ->
      ignore is_global ;
      trans_decl declc smeta sizedtype
        (Ast.map_transformation trans_expr transformation)
        identifier initial_value
  | Ast.Block stmts -> Stmt.Pattern.Block (List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Return e -> Stmt.Pattern.Return (Some (trans_expr e)) |> swrap
  | Ast.ReturnVoid -> Stmt.Pattern.Return None |> swrap
  | Ast.Break -> Stmt.Pattern.Break |> swrap
  | Ast.Continue -> Stmt.Pattern.Continue |> swrap
  | Ast.Skip -> Stmt.Pattern.Skip |> swrap

let trans_fun_def (ts : Ast.typed_statement) =
  match ts.stmt with
  | Ast.FunDef {returntype; funname; arguments; body} ->
      { FunDef.fdrt= (match returntype with UnsizedType.Void -> None | ReturnType ut -> Some ut)
      ; fdname= funname.name
      ; fdargs= List.map ~f:trans_arg arguments
      ; fdbody=
          trans_stmt
            {dread= None; dconstrain= None; dadlevel= AutoDiffable}
            body
          |> unwrap_block_or_skip
      ; fdloc = ts.smeta.loc }
  | _ ->
      raise_s
        [%message "Found non-function definition statement in function block"]

let gen_write decl_id sizedtype =
  let bodyfn var =
    { Stmt.stmt= 
        Stmt.Pattern.NRFunApp (CompilerInternal, string_of_internal_fn FnWriteParam, [var])
    ; smeta= no_span }
  in
  for_scalar sizedtype bodyfn
    { expr= Var decl_id
    ; emeta= {internal_meta with mtype= remove_size sizedtype} }
    no_span

let gen_writes block_filter vars =
  List.filter_map
    ~f:(function
      | decl_id, (st, block) when block = block_filter ->
          Some (gen_write decl_id st)
      | _ -> None)
    vars

let compiler_if compiler_internal_var stmts =
  let body =
    match stmts with
    | [({Stmt.stmt= Stmt.Pattern.Block _; _} as s)] -> s
    | ls -> {stmt= Block ls; smeta= no_span}
  in
  let cond = {Expr.expr= Var compiler_internal_var; emeta= internal_meta} in
  match stmts with
  | [] -> []
  | _ -> [{Stmt.stmt= IfElse (cond, body, None); smeta= no_span}]

let get_block block prog =
  match block with
  | Program.Parameters -> prog.Ast.parametersblock
  | TransformedParameters -> prog.transformedparametersblock
  | GeneratedQuantities -> prog.generatedquantitiesblock
  | Data -> prog.datablock

let trans_prog filename p : Program.Typed.t =
  (*
     1. prepare_params: add read_param calls (same call should constrain?)
          maybe read(constrained()), constrain(read()), or read("constraint", ...)
     1. prepare_params: add tparams 's; add checks
     2. transform_inits: add read_param calls (same call should unconstrain?)
     3. prepare_data: add read_data calls and checks
     4. prepare_data: add tdata 's and checks
     5. add write() calls to generate_quantities for params, tparams...
             shit these are conditional depending on the flag.
             add the flag to the call to write?
           apparently tdata aren't written anywhere

     during code gen:
     get_param_names: scan prepare_params for Decl at top level
     constrained_param_names: needs to tell between tparams and gqs and not
     unconstrained param names: same, but also some funky
        adjustments for unconstrained space: ???
*)
  let { Ast.functionblock
      ; datablock
      ; transformeddatablock
      ; parametersblock
      ; transformedparametersblock
      ; modelblock; _ } =
    p
  in
  let map f list_op = Option.value ~default:[] list_op |> List.concat_map ~f in
  let grab_names_sizes block =
    let get_name_size s =
      match s.Ast.stmt with
      | Ast.VarDecl {sizedtype; identifier; _} ->
          Some (identifier.name, (trans_sizedtype sizedtype, block))
      | _ -> None
    in
    List.map ~f:get_name_size (Option.value ~default:[] (get_block block p))
  in
  let output_vars =
    [ grab_names_sizes Program.Parameters
    ; grab_names_sizes Program.TransformedParameters
    ; grab_names_sizes Program.GeneratedQuantities ]
    |> List.concat |> List.filter_opt
  and input_vars = grab_names_sizes Program.Data |> List.filter_opt in
  let datab =
    map
      (trans_stmt
         {dread= Some ReadData; dconstrain= Some Check; dadlevel= DataOnly})
      datablock
  in
  let prepare_data =
    datab
    @ map
        (trans_stmt {dread= None; dconstrain= Some Check; dadlevel= DataOnly})
        transformeddatablock
  in
  let modelb =
    map
      (trans_stmt {dread= None; dconstrain= None; dadlevel= AutoDiffable})
      modelblock
  in
  let log_prob =
    map
      (trans_stmt
         { dread= Some ReadParam
         ; dconstrain= Some Constrain
         ; dadlevel= AutoDiffable })
      parametersblock
    @ map
        (trans_stmt
           {dread= None; dconstrain= Some Check; dadlevel= AutoDiffable})
        transformedparametersblock
    @
    match modelb with
    | [] -> []
    | hd :: _ -> [{stmt= Block modelb; smeta= hd.smeta}]
  in
  let gen_from_block declc block =
    map (trans_stmt declc) (get_block block p) @ gen_writes block output_vars
  in
  let part_decls block =
    List.partition_tf
      ~f:(function {stmt= Decl _; _} -> true | _ -> false)
      (gen_from_block
         {dread= None; dconstrain= Some Check; dadlevel= DataOnly}
         block)
  in
  let txparam_decls, txparam_stmts = part_decls TransformedParameters in
  let gq_decls, gq_stmts = part_decls GeneratedQuantities in
  let generate_quantities =
    gen_from_block
      {dread= Some ReadParam; dconstrain= Some Constrain; dadlevel= DataOnly}
      Parameters
    @ txparam_decls
    @ compiler_if "emit_transformed_parameters__" txparam_stmts
    @ gq_decls
    @ compiler_if "emit_generated_quantities__" gq_stmts
  in
  let transform_inits =
    map
      (trans_stmt
         { dread= Some ReadData
         ; dconstrain= Some Unconstrain
         ; dadlevel= DataOnly })
      parametersblock
  in
  { functions_block=
      Option.value_map functionblock ~default:[] ~f:(List.map ~f:trans_fun_def)
  ; input_vars
  ; prepare_data
  ; log_prob
  ; generate_quantities
  ; transform_inits
  ; output_vars
  ; prog_name= !Semantic_check.model_name
  ; prog_path= filename }

(*===================== tests =========================================*)

let mir_from_string s =
  let untyped_prog =
    Parse.parse_string Parser.Incremental.program s
    |> Result.map_error ~f:Parse.render_syntax_error
    |> Result.ok_or_failwith
  in
  let typed_prog_result = Semantic_check.semantic_check_program untyped_prog in
  let typed_prog =
    typed_prog_result
    |> Result.map_error ~f:(function
         | x :: _ -> (Semantic_error.pp |> Fmt.to_to_string) x
         | _ -> failwith "mir_from_string: can't happen" )
    |> Result.ok_or_failwith
  in
  trans_prog "" typed_prog

let%expect_test "Prefix-Op-Example" =
  let mir =
    mir_from_string
      {|
        model {
          int i;
          if (i < -1)
            print("Badger");
        }
      |}
  in
  let op = mir.log_prob in
  print_s [%sexp (op : stmt_loc list)] ;
  (* Perhaps this is producing too many nested lists. XXX*)
  [%expect
    {|
      ((Block
        ((Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt)))
         (IfElse
          (FunApp StanLib Less__ ((Var i) (FunApp StanLib PMinus__ ((Lit Int 1)))))
          (NRFunApp CompilerInternal FnPrint__ ((Lit Str Badger))) ())))) |}]

let%expect_test "read data" =
  let m = mir_from_string "data { matrix[10, 20] mat[5]; }" in
  print_s [%sexp (m.prepare_data : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype DataOnly) (decl_id mat)
      (decl_type
       (Sized (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5)))))
     (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
      (body
       (Block
        ((For (loopvar sym2__) (lower (Lit Int 1))
          (upper (FunApp StanLib Times__ ((Lit Int 10) (Lit Int 20))))
          (body
           (Block
            ((Assignment (mat ((Single (Var sym1__)) (Single (Var sym2__))))
              (Indexed
               (FunApp CompilerInternal FnReadData__
                ((Lit Str mat) (Lit Str matrix) (Lit Int 10) (Lit Int 20)))
               ((Single (Var sym1__)) (Single (Var sym2__)))))))))))))) |}]

let%expect_test "read param" =
  let m = mir_from_string "parameters { matrix<lower=0>[10, 20] mat[5]; }" in
  print_s [%sexp (m.log_prob : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype AutoDiffable) (decl_id mat)
      (decl_type
       (Sized (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5)))))
     (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
      (body
       (Block
        ((Assignment (mat ((Single (Var sym1__))))
          (Indexed
           (FunApp CompilerInternal FnReadParam__
            ((Lit Str mat) (Lit Str matrix) (Lit Int 10) (Lit Int 20)))
           ((Single (Var sym1__)))))))))
     (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
      (body
       (Block
        ((For (loopvar sym2__) (lower (Lit Int 1))
          (upper (FunApp StanLib Times__ ((Lit Int 10) (Lit Int 20))))
          (body
           (Block
            ((Assignment (mat ((Single (Var sym1__)) (Single (Var sym2__))))
              (FunApp CompilerInternal FnConstrain__
               ((Indexed (Var mat) ((Single (Var sym1__)) (Single (Var sym2__))))
                (Lit Str lb) (Lit Int 0))))))))))))) |}]

let%expect_test "gen quant" =
  let m =
    mir_from_string "generated quantities { matrix<lower=0>[10, 20] mat[5]; }"
  in
  print_s [%sexp (m.generate_quantities : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype DataOnly) (decl_id mat)
      (decl_type
       (Sized (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5)))))
     (IfElse (Var emit_generated_quantities__)
      (Block
       ((For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
         (body
          (Block
           ((For (loopvar sym2__) (lower (Lit Int 1))
             (upper (FunApp StanLib Times__ ((Lit Int 10) (Lit Int 20))))
             (body
              (Block
               ((NRFunApp CompilerInternal FnCheck__
                 ((Lit Str greater_or_equal) (Lit Str "mat[sym1__, sym2__]")
                  (Indexed (Var mat)
                   ((Single (Var sym1__)) (Single (Var sym2__))))
                  (Lit Int 0)))))))))))
        (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
         (body
          (Block
           ((For (loopvar sym2__) (lower (Lit Int 1))
             (upper (FunApp StanLib Times__ ((Lit Int 10) (Lit Int 20))))
             (body
              (Block
               ((NRFunApp CompilerInternal FnWriteParam__
                 ((Indexed (Var mat)
                   ((Single (Var sym1__)) (Single (Var sym2__))))))))))))))))
      ())) |}]

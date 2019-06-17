open Core_kernel

module type S = sig
  type labels
  type properties

  val transfer_function : labels -> properties -> properties
end



module StringSet = Set.Make_using_comparator (String)

module Expression = struct
  let rec free_vars_helper accu (expression : Middle.Expr.Typed.t) =
    match expression.expr with
    | Middle.Expr.ExprF.Var x -> StringSet.add accu x
    | Lit _ -> accu
    | FunApp (_, f, l) ->
        List.fold l ~init:(StringSet.add accu f) ~f:free_vars_helper
    | TernaryIf (pe, te, fe) ->
        free_vars_helper (free_vars_helper (free_vars_helper accu pe) te) fe
    | Indexed (e, idxs) ->
        List.fold idxs ~init:(free_vars_helper accu e) ~f:free_vars_idxs
    | EAnd (e1, e2) | EOr (e1, e2) ->
        free_vars_helper (free_vars_helper accu e1) e2

  and free_vars_idxs accu indexed_expr =
    match indexed_expr with
    | Middle.IndexedExpr.All -> accu
    | Single e | Upfrom e | Downfrom e | MultiIndex e ->
        free_vars_helper accu e
    | Between (e1, e2) -> free_vars_helper (free_vars_helper accu e1) e2

  let free_vars = free_vars_helper StringSet.empty
end

(* -- Statements ------------------------------------------------------------ *)
module Statement = struct
  let rec free_vars_helper ~accu
      (stmt : Middle.Statement.Typed.t) =
    match stmt with
    | Middle.Statement.Assignment ((_, []), e) | Return (Some e) | TargetPE e ->
        Expression.free_vars_helper accu e
    | Assignment ((_, l), e) ->
        List.fold l
          ~init:(Expression.free_vars_helper accu e)
          ~f:Expression.free_vars_idxs
    | NRFunApp (_, f, l) ->
        List.fold l ~init:(StringSet.add accu f) ~f:Expression.free_vars_helper
    | IfElse (e, {stmt= ts; _}, Some {stmt= fs; _}) ->
        free_vars_helper
          ~accu:
            (free_vars_helper ~accu:(Expression.free_vars_helper accu e) ts)
          fs
    | IfElse (e, {stmt= ts; _}, None) | While (e, {stmt= ts; _}) ->
        free_vars_helper ~accu:(Expression.free_vars_helper accu e) ts
    | For {lower; upper; body; _} ->
        free_vars_helper
          ~accu:
            (Expression.free_vars_helper
               (Expression.free_vars_helper accu lower)
               upper)
          body.stmt
    | Block xs | SList xs ->
        List.fold xs ~init:accu ~f:(fun accu {stmt; _} ->
            free_vars_helper ~accu stmt )
    | Decl _ | Break | Continue | Return None | Skip -> accu

  (* let top_free_vars (flowgraph_to_mir: (int,Middle.stmt_loc_num,Int.comparator_witness) Map.t) (stmt : (Middle.expr_typed_located,int) Middle.statement) = 
        match stmt with 
        | Middle.Assignment _ | Return _ | TargetPE _ 
        | NRFunApp _ | Decl _| Break | Continue | Skip -> 
            free_vars_helper 
                ~accu:StringSet.empty
                (Mir_utils.statement_stmt_loc_of_statement_stmt_loc_num flowgraph_to_mir stmt)
        
        | Middle.While (e, _) | Middle.IfElse (e, _, _) -> 
            Expression.free_vars_helper StringSet.empty e

        | Middle.For {lower= e1; upper= e2; _} ->
            Expression.free_vars_helper (Expression.free_vars_helper StringSet.empty e1) e2

        | Middle.Block _ | Middle.SList _ -> 
            StringSet.empty *)
  
  (* let live_variables_tf (flowgraph_to_mir: (int,Middle.stmt_loc_num,Int.comparator_witness) Map.t) lbl props = 
    let mir_node = Map.find flowgraph_to_mir lbl  *)
end

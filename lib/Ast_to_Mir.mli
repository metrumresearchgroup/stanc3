(** Translate from the AST to the MIR *)

val for_scalar :
     Ast.unsizedtype
  -> (Mir.expr_typed_located -> Mir.stmt_loc)
  -> Mir.expr_typed_located
  -> Mir.stmt_loc
(** [for_scalar] is a helper to generate MIR for loops around scalars
    in an unsized type.*)

val for_eigen :
     Ast.unsizedtype
  -> (Mir.expr_typed_located -> Mir.stmt_loc)
  -> Mir.expr_typed_located
  -> Mir.stmt_loc
(** [for_eigen] is a helper to generate MIR for loops around Eigen collections
    in an unsized type. The for loop will bottom out and run at the
    entire Eigen collection level.*)

val trans_prog :
     string
  -> Ast.typed_program
  -> (Mir.expr_typed_located, Mir.stmt_loc) Mir.prog
(** [trans_prog] is the main workhorse converting from an AST to MIR*)

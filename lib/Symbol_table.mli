(** Symbol table interface to implement var map *)

val unsafe_clear_symbol_table : unit -> unit
(** Clears the whole symbol table *)

val enter : string -> Ast.originblock * Ast.unsizedtype -> unit
(** Enters a specified identifier with its specified type (or other) information
    into a symbol table  *)

val look : string -> (Ast.originblock * Ast.unsizedtype) option
(** Looks for an identifier in a symbol table and returns its information if found and None otherwise  *)

val begin_scope : unit -> unit
(** Used to start a new local scope which symbols added from now will end up in *)

val end_scope : unit -> unit
(** Used to end a local scope, purging the symbol table of all symbols added in that scope *)

val set_read_only : string -> unit
(** Used to add a read only label to an identifier *)

val get_read_only : string -> bool
(** Used to check for a read only label for an identifier *)

val set_is_assigned : string -> unit
(** Label an identifier as having been assigned to *)

val set_is_unassigned : string -> unit
(** Label an identifier as not having been assigned to *)

val check_is_unassigned : string -> bool
(** Check whether an identifier is labelled as unassigned *)

val check_some_id_is_unassigned : unit -> bool
(** Used to check whether some identifier is labelled as unassigned *)

val is_global : string -> bool
(** Used to check whether an identifier was declared in global scope *)

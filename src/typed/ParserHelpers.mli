(** [from_file filename] parses the contents of [filename] and produces an
    AST. *)
val from_file : string -> Syntax.exp

(** [from_string str] parses [str] and produces an AST. *)
val from_string : string -> Syntax.exp
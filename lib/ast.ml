type binary_op =
  | Add | Subtract | Multiply | Divide
  | Equal | NotEqual | LessThan | GreaterThan

type unary_op =
  | Negate

type expr =
  | Literal of value
  | Variable of string
  | Binary of expr * binary_op * expr
  | Unary of unary_op * expr
  | Call of expr * expr list
  | FunctionExpr of string list * stmt list

and stmt =
  | ExprStmt of expr
  | Assignment of string * expr
  | IfStmt of expr * stmt list * stmt list option
  | WhileStmt of expr * stmt list
  | Return of expr option

and value =
  | Number of float
  | String of string
  | Bool of bool
  | Nil

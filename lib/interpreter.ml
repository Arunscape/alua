open Ast

type environment = (string, value) Hashtbl.t

let rec eval_expr (env : environment) (expr : expr) : value =
  match expr with
  | Literal v -> v
  | Variable name -> Hashtbl.find env name
  | Binary (left, op, right) -> eval_binary env left op right
  | Unary (op, expr) -> eval_unary env op expr
  | _ -> failwith "Not implemented"

and eval_binary env left op right =
  let left_val = eval_expr env left in
  let right_val = eval_expr env right in
  match (left_val, op, right_val) with
  | (Number l, Add, Number r) -> Number (l +. r)
  | (Number l, Subtract, Number r) -> Number (l -. r)
  | (Number l, Multiply, Number r) -> Number (l *. r)
  | (Number l, Divide, Number r) -> Number (l /. r)
  | (Number l, LessThan, Number r) -> Bool (l < r)
  | (Number l, GreaterThan, Number r) -> Bool (l > r)
  | (l, Equal, r) -> Bool (l = r)
  | (l, NotEqual, r) -> Bool (l <> r)
  | _, _, _ -> failwith "Invalid binary operation"

and eval_unary env op expr =
    let value = eval_expr env expr in
    match (op, value) with
    | (Negate, Number n) -> Number (-.n)
    | _, _ -> failwith "Invalid unary operation"

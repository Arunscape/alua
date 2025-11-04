type t = Number of float | String of string | Bool of bool | Nil

let to_string = function
  | Number n -> string_of_float n
  | String s -> s
  | Bool true -> "true"
  | Bool false -> "false"
  | Nil -> "nil"

let to_number = function
  | Number n -> n
  | String s -> ( try float_of_string s with _ -> 0.0)
  | Bool true -> 1.0
  | Bool false -> 0.0
  | Nil -> 0.0

let is_truthy = function
  | Nil -> false
  | Bool false -> false
  | Number 0.0 -> false
  | _ -> true

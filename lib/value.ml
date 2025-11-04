type t = Number of float | String of string | Nil

let to_string = function
  | Number n -> string_of_float n
  | String s -> s
  | Nil -> "nil"

let to_number = function
  | Number n -> n
  | String s -> ( try float_of_string s with _ -> 0.0)
  | Nil -> 0.0

let is_truthy = function Nil -> false | Number 0.0 -> false | _ -> true

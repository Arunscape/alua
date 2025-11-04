open Alua

(* Helper function to parse and evaluate *)
let eval input =
  let lexbuf = Lexing.from_string input in
  Parser.main Lexer.token lexbuf

(* Helper to check if variable exists in env *)
let get_var name =
  try Some (Hashtbl.find Interpreter.env name) with Not_found -> None

(* Clear environment between tests *)
let clear_env () = Hashtbl.clear Interpreter.env

(* Tests for basic arithmetic *)
let test_arithmetic () =
  clear_env ();
  (* Just make sure it doesn't crash *)
  eval "2 + 3";
  eval "10 * 5";
  eval "(2 + 3) * 4"

(* Tests for variable assignment *)
let test_assignment () =
  clear_env ();

  (* Assign a variable *)
  eval "x = 5";
  assert (get_var "x" = Some 5.0);

  (* Assign another variable *)
  eval "y = 10";
  assert (get_var "y" = Some 10.0);

  (* Reassign a variable *)
  eval "x = 20";
  assert (get_var "x" = Some 20.0);

  print_endline "✓ Assignment tests passed"

(* Tests for variable lookup *)
let test_variable_lookup () =
  clear_env ();

  (* Set up variables *)
  eval "a = 5";
  eval "b = 3";

  (* Use variables in expressions *)
  eval "a + b";
  eval "a * b";
  eval "a + b * 2";

  print_endline "✓ Variable lookup tests passed"

(* Test variable in assignment expression *)
let test_variable_in_assignment () =
  clear_env ();

  eval "x = 10";
  eval "y = x + 5";
  assert (get_var "y" = Some 15.0);

  eval "z = x * y";
  assert (get_var "z" = Some 150.0);

  print_endline "✓ Variable in assignment tests passed"

(* Test unknown variable error *)
let test_unknown_variable () =
  clear_env ();

  try
    eval "unknown_var + 5";
    assert false (* Should have thrown an error *)
  with Failure msg ->
    assert (String.sub msg 0 17 = "Unknown variable:");
    print_endline "✓ Unknown variable test passed"

(* Run all tests *)
let () =
  print_endline "Running tests...\n";
  test_arithmetic ();
  test_assignment ();
  test_variable_lookup ();
  test_variable_in_assignment ();
  test_unknown_variable ();
  print_endline "\n✅ All tests passed!"

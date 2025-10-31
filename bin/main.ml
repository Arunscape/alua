open Alua

let parse_and_eval input =
  let lexbuf = Lexing.from_string input in
  try
    let result = Parser.main Lexer.token lexbuf in
    Printf.printf "= %.2f\n" result
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | Parser.Error -> Printf.printf "Syntax error\n"

let () =
  print_endline "Arun's Lua Interpreter that is just a calculator for now";
  print_endline "Press Ctrl+D to exit\n";
  let rec repl () =
    print_string "> ";
    flush stdout;
    match read_line () with
    | input ->
        parse_and_eval input;
        repl ()
    | exception End_of_file ->
        print_endline "\nGoodbye!";
        ()
  in
  repl ()


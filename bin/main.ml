open Alua

let parse_and_eval input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.program Lexer.token lexbuf;
    flush stdout
  with
  | Failure msg ->
      Printf.printf "Error: %s\n" msg;
      flush stdout
  | Parser.Error ->
      Printf.printf "Syntax error\n";
      flush stdout

let () =
  print_endline "Arun's Lua Interpreter that is just a calculator for now";
  print_endline "Press Ctrl+D to exit\n";

  (* Set up history file *)
  let history_file = ".alua_history" in
  let _ = LNoise.history_load ~filename:history_file in
  let _ = LNoise.history_set ~max_length:100 in

  let rec repl () =
    match LNoise.linenoise "> " with
    | None ->
        print_endline "\nGoodbye!";
        LNoise.history_save ~filename:history_file |> ignore
    | Some input ->
        if String.trim input <> "" then (
          LNoise.history_add input |> ignore;
          parse_and_eval input;
          flush stdout);
        repl ()
  in
  repl ()

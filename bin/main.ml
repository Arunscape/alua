open Alua

let parse_and_eval input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.program Lexer.token lexbuf;
    flush stdout;
    true
  with
  | Failure msg ->
      Printf.printf "Error: %s\n" msg;
      flush stdout;
      false
  | Parser.Error ->
      Printf.printf "Syntax error\n";
      flush stdout;
      false

let count_keywords line =
  let words = String.split_on_char ' ' (String.trim line) in
  let rec count_depth acc = function
    | [] -> acc
    | word :: rest ->
        let trimmed = String.trim word in
        let delta =
          match trimmed with
          | "if" | "while" -> 1
          | "then" | "do" -> 0 (* Don't double-count *)
          | "end" -> -1
          | "else" -> 0 (* else doesn't change depth *)
          | _ -> 0
        in
        count_depth (acc + delta) rest
  in
  count_depth 0 words

let rec read_complete acc depth =
  let prompt = if depth > 0 then "  " else "> " in
  match LNoise.linenoise prompt with
  | None -> if acc <> "" then Some acc else None
  | Some line ->
      let trimmed = String.trim line in

      if trimmed = "" && depth > 0 then read_complete acc depth
      else if trimmed = "" && depth = 0 && acc <> "" then Some acc
      else
        let new_acc = acc ^ line ^ "\n" in
        let new_depth = depth + count_keywords line in

        if new_depth <= 0 && acc <> "" then Some new_acc
        else read_complete new_acc new_depth

let () =
  print_endline "Arun's Lua Interpreter";
  print_endline "Press Ctrl+D to exit\n";

  let history_file = ".alua_history" in
  let _ = LNoise.history_load ~filename:history_file in
  let _ = LNoise.history_set ~max_length:100 in

  let rec repl () =
    match read_complete "" 0 with
    | None ->
        print_endline "\nGoodbye!";
        LNoise.history_save ~filename:history_file |> ignore
    | Some input ->
        LNoise.history_add input |> ignore;
        let _ = parse_and_eval input in
        repl ()
  in
  repl ()

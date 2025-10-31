let () =
  print_endline "Arun's Lua Interpreter";
  let rec repl () =
    print_string "> ";
    flush stdout;
    match read_line () with
    | input ->
        (try
           let tokens = Alua.Lexer.tokenize input in
           List.iter (fun token ->
             match token with
             | Alua.Token.Number n -> Printf.printf "NUMBER(%f) " n
             | Alua.Token.Plus -> print_string "PLUS "
             | Alua.Token.Minus -> print_string "MINUS "
             | Alua.Token.Star -> print_string "STAR "
             | Alua.Token.Identifier id -> Printf.printf "ID(%s) " id
             | Alua.Token.EOF -> print_string "EOF"
             | _ -> print_string "TOKEN "
           ) tokens;
           print_newline ()
         with e ->
           Printf.printf "Error: %s\n" (Printexc.to_string e));
        repl ()
    | exception End_of_file ->
        print_endline "\nGoodbye!";
        ()
  in
  repl ()


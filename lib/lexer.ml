open Token

type lexer = {
  input : string;
  mutable position : int;
  mutable current_char : char option;
}

let create input =
  let lexer = {
    input;
    position = 0;
    current_char = if String.length input > 0 then Some input.[0] else None;
  } in
  lexer

let advance lexer =
  lexer.position <- lexer.position + 1;
  if lexer.position < String.length lexer.input then
    lexer.current_char <- Some lexer.input.[lexer.position]
  else
    lexer.current_char <- None

let peek lexer =
  if lexer.position + 1 < String.length lexer.input then
    Some lexer.input.[lexer.position + 1]
  else
    None

let skip_whitespace lexer =
  let rec loop () =
    match lexer.current_char with
    | Some (' ' | '\t' | '\n' | '\r') ->
        advance lexer;
        loop ()
    | _ -> ()
  in
  loop ()

let read_number lexer =
  let start_pos = lexer.position in
  let rec loop () =
    match lexer.current_char with
    | Some ('0'..'9' | '.') ->
        advance lexer;
        loop ()
    | _ -> ()
  in
  loop ();
  let num_str = String.sub lexer.input start_pos (lexer.position - start_pos) in
  float_of_string num_str

let read_identifier lexer =
  let start_pos = lexer.position in
  let rec loop () =
    match lexer.current_char with
    | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') ->
        advance lexer;
        loop ()
    | _ -> ()
  in
  loop ();
  String.sub lexer.input start_pos (lexer.position - start_pos)

let keyword_or_identifier str =
  match str with
  | "if" -> If
  | "then" -> Then
  | "else" -> Else
  | "end" -> End
  | "while" -> While
  | "do" -> Do
  | "function" -> Function
  | "return" -> Return
  | "local" -> Local
  | "true" -> True
  | "false" -> False
  | "nil" -> Nil
  | _ -> Identifier str

let next_token lexer =
  skip_whitespace lexer;
  match lexer.current_char with
  | None -> EOF
  | Some '+' -> advance lexer; Plus
  | Some '-' -> advance lexer; Minus
  | Some '*' -> advance lexer; Star
  | Some '/' -> advance lexer; Slash
  | Some '(' -> advance lexer; LeftParen
  | Some ')' -> advance lexer; RightParen
  | Some '{' -> advance lexer; LeftBrace
  | Some '}' -> advance lexer; RightBrace
  | Some '<' -> advance lexer; LessThan
  | Some '>' -> advance lexer; GreaterThan
  | Some '=' ->
      advance lexer;
      (match lexer.current_char with
       | Some '=' -> advance lexer; EqualEqual
       | _ -> Equal)
  | Some '~' ->
      advance lexer;
      (match lexer.current_char with
       | Some '=' -> advance lexer; NotEqual
       | _ -> failwith "Unexpected character after ~")
  | Some ('0'..'9') ->
      let num = read_number lexer in
      Number num
  | Some ('a'..'z' | 'A'..'Z' | '_') ->
      let id = read_identifier lexer in
      keyword_or_identifier id
  | Some c -> failwith (Printf.sprintf "Unexpected character: %c" c)

let tokenize input =
  let lexer = create input in
  let rec loop acc =
    let token = next_token lexer in
    match token with
    | EOF -> List.rev (EOF :: acc)
    | _ -> loop (token :: acc)
  in
  loop []

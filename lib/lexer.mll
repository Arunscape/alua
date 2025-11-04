{
open Parser
}

(* Regular expression definitions *)
let digit  = ['0'-'9']
let number = digit+ ('.' digit+)?
let white  = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z' '_']
let ident  = letter (letter | digit)*

rule token = parse
  (* Whitespace *)
  | white       { token lexbuf }
  
  (* String literals *)
  | '"'         { read_string (Buffer.create 16) lexbuf }
  
  (* Keywords - MUST come before identifiers *)
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "end"       { END }
  | "while"     { WHILE }
  | "do"        { DO }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "print"     { PRINT }
  | "nil"       { NIL }
  
  (* Identifiers and numbers *)
  | ident       { IDENT (Lexing.lexeme lexbuf) }
  | number      { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  
  (* Multi-character operators - MUST come before single-char *)
  | ".."        { CONCAT }
  | "=="        { EQEQ }
  | "~="        { NEQ }
  | "<="        { LEQ }
  | ">="        { GEQ }
  
  (* Single-character operators *)
  | '='         { ASSIGN }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIV }
  | '%'         { MOD }
  | '<'         { LT }
  | '>'         { GT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | ','         { COMMA }
  
  (* End of file and errors *)
  | eof         { EOF }
  | _           { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

and read_string buf = parse
  | '"'         { STRING (Buffer.contents buf) }
  | '\\' '"'    { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' 'n'    { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'    { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\\'   { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+ 
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof         { failwith "Unterminated string" }


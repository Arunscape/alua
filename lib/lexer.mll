{
open Parser
}

let digit = ['0'-'9']
let number = digit+ ('.' digit+)?
let white = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter (letter | digit)*

rule token = parse
  | white      { token lexbuf }
    | '"' { read_string (Buffer.create 16) lexbuf } 
    (*Keywords*)
      | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | "end"      { END }
  | "while"    { WHILE }
  | "do"       { DO }
  | "true"     { TRUE }
  | "false"    { FALSE }
    | "print" { PRINT } 
    | "nil" { NIL }

(*identifiers*)
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | number     { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }


(*multichar operators*)
  | ".."       { CONCAT }
  | "=="       { EQEQ }
  | "~="       { NEQ }
  | "<="       { LEQ }
  | ">="       { GEQ }

(*operators*)
  | '='        { ASSIGN }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIV }
  | '<'        { LT }
  | '>'        { GT }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | ','        { COMMA }
  (**)
  | eof        { EOF }
  | _          { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

  and read_string buf = parse
  | '"'        { STRING (Buffer.contents buf) }
  | '\\' '"'   { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' 'n'   { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'   { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\\'  { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+ 
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof        { failwith "Unterminated string" }

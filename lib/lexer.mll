{
open Parser
}

let digit = ['0'-'9']
let number = digit+ ('.' digit+)?
let white = [' ' '\t' '\n' '\r']+

rule token = parse
  | white      { token lexbuf }
  | number     { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIV }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | eof        { EOF }
  | _          { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

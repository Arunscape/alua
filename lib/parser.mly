%token <float> NUMBER
%token <string> IDENT
%token ASSIGN
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIV

%start <unit> main

%%

main:
  | s = statement EOF { s }

statement:
  | x = IDENT ASSIGN e = expr 
    { 
      Hashtbl.replace Interpreter.env x e;
      Printf.printf "%s = %.2f\n" x e 
    }
  | e = expr { Printf.printf "= %.2f\n" e }

expr:
  | n = NUMBER { n }
  | x = IDENT {
    try Hashtbl.find Interpreter.env x
    with Not_found -> failwith ( "Unknown variable: " ^ x)
}
  | LPAREN e = expr RPAREN { e }
  | e1 = expr PLUS e2 = expr { e1 +. e2 }
  | e1 = expr MINUS e2 = expr { e1 -. e2 }
  | e1 = expr TIMES e2 = expr { e1 *. e2 }
  | e1 = expr DIV e2 = expr { e1 /. e2 }

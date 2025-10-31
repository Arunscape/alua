%token <float> NUMBER
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIV

%start <float> main

%%

main:
  | e = expr EOF { e }

expr:
  | n = NUMBER { n }
  | LPAREN e = expr RPAREN { e }
  | e1 = expr PLUS e2 = expr { e1 +. e2 }
  | e1 = expr MINUS e2 = expr { e1 -. e2 }
  | e1 = expr TIMES e2 = expr { e1 *. e2 }
  | e1 = expr DIV e2 = expr { e1 /. e2 }

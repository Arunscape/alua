%token <float> NUMBER
%token <string> STRING
%token <string> IDENT
%token ASSIGN CONCAT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN COMMA
%token PRINT NIL
%token EOF

%left CONCAT
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
      Printf.printf "%s = %s\n" x (Value.to_string e) 
    }
  | PRINT LPAREN e = expr RPAREN
    {
      print_endline (Value.to_string e)
    }
  | e = expr { Printf.printf "%s\n" (Value.to_string e) }

expr:
  | n = NUMBER { Value.Number n }
  | s = STRING { Value.String s }
  | NIL { Value.Nil }
  | x = IDENT 
    { 
      try Hashtbl.find Interpreter.env x
      with Not_found -> failwith ("Unknown variable: " ^ x)
    }
  | LPAREN e = expr RPAREN { e }
  | e1 = expr PLUS e2 = expr 
    { 
      Value.Number (Value.to_number e1 +. Value.to_number e2) 
    }
  | e1 = expr MINUS e2 = expr 
    { 
      Value.Number (Value.to_number e1 -. Value.to_number e2) 
    }
  | e1 = expr TIMES e2 = expr 
    { 
      Value.Number (Value.to_number e1 *. Value.to_number e2) 
    }
  | e1 = expr DIV e2 = expr 
    { 
      Value.Number (Value.to_number e1 /. Value.to_number e2) 
    }
  | e1 = expr CONCAT e2 = expr
    {
      Value.String (Value.to_string e1 ^ Value.to_string e2)
    }

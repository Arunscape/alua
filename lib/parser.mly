(* Token declarations *)
%token <float> NUMBER
%token <string> STRING
%token <string> IDENT

%token ASSIGN CONCAT
%token PLUS MINUS TIMES DIV MOD
%token LPAREN RPAREN COMMA
%token PRINT NIL EOF

%token IF THEN ELSE END
%token WHILE DO
%token TRUE FALSE

%token EQEQ NEQ LT GT LEQ GEQ
%token SEMICOLON

(* Operator precedence and associativity *)
%left CONCAT
%left EQEQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD

(* Start symbol *)
%start <unit> program

%%

(* ========== PROGRAM STRUCTURE ========== *)

program:
  | stmts = statement_list EOF
    { List.iter (fun f -> f ()) stmts }

statement_list:
  | { [] }
  | s = statement stmts = statement_list
    { s :: stmts }

(* ========== STATEMENTS ========== *)

statement:
  (* Variable assignment *)
  | x = IDENT ASSIGN e = expr
    {
      fun () ->
        let v = e () in
        Hashtbl.replace Interpreter.env x v
    }
  
  (* If statement *)
  | IF cond = expr THEN then_stmts = statement_list else_part = else_clause END
    {
      fun () ->
        if Value.is_truthy (cond ()) then
          List.iter (fun f -> f ()) then_stmts
        else
          List.iter (fun f -> f ()) else_part
    }
  
  (* While loop *)
  | WHILE cond = expr DO body = statement_list END
    {
      fun () ->
        while Value.is_truthy (cond ()) do
          List.iter (fun f -> f ()) body
        done
    }
  
  (* Print function *)
  | PRINT LPAREN e = expr RPAREN
    {
      fun () ->
        print_endline (Value.to_string (e ()))
    }
  
  (* Expression statement *)
  | e = expr
    {
      fun () ->
        Printf.printf "%s\n" (Value.to_string (e ()))
    }

else_clause:
  | { [] }
  | ELSE stmts = statement_list
    { stmts }

(* ========== EXPRESSIONS ========== *)

expr:
  (* Literals *)
  | n = NUMBER
    { fun () -> Value.Number n }
  | s = STRING
    { fun () -> Value.String s }
  | TRUE
    { fun () -> Value.Bool true }
  | FALSE
    { fun () -> Value.Bool false }
  | NIL
    { fun () -> Value.Nil }
  
  (* Variable lookup *)
  | x = IDENT
    {
      fun () ->
        try Hashtbl.find Interpreter.env x
        with Not_found -> failwith ("Unknown variable: " ^ x)
    }
  
  (* Parenthesized expression *)
  | LPAREN e = expr RPAREN
    { e }
  
  (* Arithmetic operators *)
  | e1 = expr PLUS e2 = expr
    {
      fun () ->
        Value.Number (Value.to_number (e1 ()) +. Value.to_number (e2 ()))
    }
  | e1 = expr MINUS e2 = expr
    {
      fun () ->
        Value.Number (Value.to_number (e1 ()) -. Value.to_number (e2 ()))
    }
  | e1 = expr TIMES e2 = expr
    {
      fun () ->
        Value.Number (Value.to_number (e1 ()) *. Value.to_number (e2 ()))
    }
  | e1 = expr DIV e2 = expr
    {
      fun () ->
        Value.Number (Value.to_number (e1 ()) /. Value.to_number (e2 ()))
    }
  | e1 = expr MOD e2 = expr
    {
      fun () ->
        let n1 = Value.to_number (e1 ()) in
        let n2 = Value.to_number (e2 ()) in
        Value.Number (mod_float n1 n2)
    }
  
  (* Comparison operators *)
  | e1 = expr EQEQ e2 = expr
    { fun () -> Value.Bool ((e1 ()) = (e2 ())) }
  | e1 = expr NEQ e2 = expr
    { fun () -> Value.Bool ((e1 ()) <> (e2 ())) }
  | e1 = expr LT e2 = expr
    { fun () -> Value.Bool (Value.to_number (e1 ()) < Value.to_number (e2 ())) }
  | e1 = expr GT e2 = expr
    { fun () -> Value.Bool (Value.to_number (e1 ()) > Value.to_number (e2 ())) }
  | e1 = expr LEQ e2 = expr
    { fun () -> Value.Bool (Value.to_number (e1 ()) <= Value.to_number (e2 ())) }
  | e1 = expr GEQ e2 = expr
    { fun () -> Value.Bool (Value.to_number (e1 ()) >= Value.to_number (e2 ())) }
  
  (* String concatenation *)
  | e1 = expr CONCAT e2 = expr
    {
      fun () ->
        Value.String (Value.to_string (e1 ()) ^ Value.to_string (e2 ()))
    }


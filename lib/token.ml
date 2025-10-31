type token =
  | Number of float
  | String of string
  | Identifier of string
  | Plus | Minus | Star | Slash
  | Equal | EqualEqual | NotEqual
  | LessThan | GreaterThan
  | LeftParen | RightParen
  | LeftBrace | RightBrace
  | If | Then | Else | End
  | While | Do
  | Function | Return | Local
  | True | False | Nil
  | EOF

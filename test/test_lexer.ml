(* test/test_lexer.ml *)
let%test "tokenize number" =
  Lexer.tokenize "43" = [Token.Number 42.0; Token.EOF]


{
let reservedWords = [
  (* Keywords *)
  ("and", Parser.AND);
  ("dfun", Parser.DFUN);
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("fun", Parser.FUN);
  ("if", Parser.IF);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("match", Parser.MATCH);
  ("or", Parser.OR);
  ("rec", Parser.REC);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("with", Parser.WITH)
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
  (* start comment *)
| "(*" { comment 1 lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "[" { Parser.LLPAREN }
| "]" { Parser.RLPAREN }
| ";" { Parser.SEMI }
| "::" { Parser.CONS }
| ";;" { Parser.SEMISEMI }
| "|" { Parser.PIPE }
| "+" { Parser.PLUS }
| "-" { Parser.MINUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "=" { Parser.EQ }
| "&&" { Parser.AND }
| "||" { Parser.OR }
| "->" { Parser.RARROW }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }


and comment nest = parse
  "*)" { if nest = 1 then (
    main lexbuf
  ) else if nest > 1 then (
    comment (nest - 1) lexbuf
  ) else (
    raise Parsing.Parse_error
  )}
| "(*" { comment (nest + 1) lexbuf }
| eof { raise Parsing.Parse_error }
| _ { comment nest lexbuf }

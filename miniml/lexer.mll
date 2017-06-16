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
  ("or", Parser.OR);
  ("rec", Parser.REC);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
] 

let comment_nested = ref 0
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
  (* start comment *)
| "(*" { comment_nested := 1;
           comment lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "[" { Parser.LLPAREN }
| "]" { Parser.RLPAREN }
| "::" { Parser.CONS }
| ";;" { Parser.SEMISEMI }
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


and comment = parse
  "*)" { if !comment_nested = 1 then (
    comment_nested := 0;
    main lexbuf
  ) else if !comment_nested > 1 then (
    comment_nested := !comment_nested - 1;
    comment lexbuf
  ) else (
    raise Parsing.Parse_error
  )}
| "(*" { comment_nested := !comment_nested + 1;
         comment lexbuf }
| eof { raise Parsing.Parse_error }
| _ { comment lexbuf }

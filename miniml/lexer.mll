{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
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
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }

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

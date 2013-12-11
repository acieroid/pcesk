{
  open Tokens
  open Lexing
  exception SchemeLexingError of position
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let space = ['\n' '\t' '\r' ' ']
let special_initial = ['!' '$' '%' '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']
let initial = letter | special_initial
let special_subsequent = ['+' '-' '.' '@']
let subsequent = initial | digit | special_subsequent
let peculiar_identifier = ['+' '-']
let identifier = initial subsequent* | peculiar_identifier
let string_element = [^'"' '\\'] | "\"" | "\\"

rule lexer = parse
| eof                             { EOF }
| ';' [^'\n']* '\n'?              { lexer lexbuf }
| space+                          { lexer lexbuf }
| '('                             { LPAR }
| ')'                             { RPAR }
| '\''                            { QUOTE }
| '"' (string_element* as s) '"'  { STRING s }
| "#t"                            { BOOLEAN true}
| "#f"                            { BOOLEAN false }
| "nil"                           { NIL }
| digit+ as n                     { INTEGER (int_of_string n) }
| '-' (digit+ as n)               { INTEGER (- (int_of_string n)) }
| identifier as s                 { IDENTIFIER s }

{
 let rec to_stream lexbuf =
   try
     let item = lexer lexbuf in
     [< 'item; to_stream lexbuf >]
   with
     SchemeLexingError p ->
       print_string ("no match at line " ^ (string_of_int p.pos_lnum) ^
                     " on column " ^ (string_of_int p.pos_cnum));
       to_stream lexbuf

 let lex channel = to_stream (Lexing.from_channel channel)
 let lex_string string = to_stream (Lexing.from_string string)
 let lex_input_file file =
   let f = open_in ("input/" ^ file) in
   let res = lex f in
   close_in f;
   res
}

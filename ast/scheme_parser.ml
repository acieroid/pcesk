open Scheme_tokens
open Scheme_ast

let parse stream =
  let rec parse' tag stream = match stream with parser
    | [< 'IDENTIFIER i >] -> (Identifier i, tag+1)
    | [< 'INTEGER n >] -> (Integer n, tag+1)
    | [< 'STRING s >] -> (String s, tag+1)
    | [< 'BOOLEAN b >] -> (Boolean b, tag+1)
    (* | [< 'QUOTE; (exp, tag) = parse' >] -> List [Identifier "quote"; exp] *)
    | [< 'LPAR; (l, tag') = parseList tag >] -> (List l, tag'+1)

  and parseList tag stream = match stream with parser
    | [< 'RPAR >] -> ([], tag)
    | [< (car, tag') = parse' tag; (cdr, tag'') = parseList tag' >] -> ((car, tag') :: cdr, tag'')
  in
  parse' 0 stream

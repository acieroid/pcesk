open Scheme_tokens
open Scheme_ast

let parse stream =
  let rec parse' tag stream : scheme_node * int = match stream with parser
    | [< 'IDENTIFIER i >] -> ((Identifier i, tag+1), tag+1)
    | [< 'INTEGER n >] -> ((Integer n, tag+1), tag+1)
    | [< 'STRING s >] -> ((String s, tag+1), tag+1)
    | [< 'BOOLEAN b >] -> ((Boolean b, tag+1), tag+1)
    | [< 'LPAR; 'IDENTIFIER kwd >] -> parseKeyword tag kwd stream

  and parseKeyword tag kwd stream = match kwd with
    | "begin" -> parseBegin tag stream
    | "lambda" -> parseLambda tag stream
    | "define" -> parseDefine tag stream
    | "if" -> parseIf tag stream
    | "set!" -> parseSet tag stream
    | f -> parseFuncall tag f stream

  and parseBegin tag : scheme_token Stream.t -> scheme_node * int = parser
    | [< (body, tag') = parseList (tag+1) >] ->
      ((Begin body, tag+1), tag')

  and parseLambda tag : scheme_token Stream.t -> scheme_node * int = parser
    | [< 'LPAR; (args, tag') = parseArgs (tag+1);
         (body, tag'') = parseList tag' >] ->
      begin match body with
      | [] -> failwith "Anonymous function without body"
      | _ -> ((Lambda (args, body), tag+1), tag'')
      end

  and parseDefine tag : scheme_token Stream.t -> scheme_node * int = parser
    | [< 'LPAR; 'IDENTIFIER n; (args, tag') = parseArgs (tag+2);
         (body, tag'') = parseList tag' >] ->
      begin match body with
      | [] -> failwith ("Function without body: " ^ n)
      | _ -> ((DefineFun ((n, tag+2), args, body), tag+1), tag'')
      end
    | [< 'IDENTIFIER n; (e, tag') = parse' (tag+2) >] ->
      ((Define ((n, tag+2), e), tag+1), tag')

  and parseIf tag : scheme_token Stream.t -> scheme_node * int = parser
    | [< (cond, tag') = parse' (tag+1);
         (cons, tag'') = parse' tag';
         (alt, tag''') = parse' tag'' >] ->
      ((If (cond, cons, alt), tag+1), tag''')

  and parseSet tag : scheme_token Stream.t -> scheme_node * int = parser
    | [< 'IDENTIFIER v; (e, tag') = parse' (tag+2) >] ->
      ((Set ((v, tag+2), e), tag+1), tag')

  and parseFuncall tag f : scheme_token Stream.t -> scheme_node * int = parser
    | [< (args, tag') = parseList (tag+2) >] ->
      ((Funcall (((Identifier f), tag+2), args), tag+1), tag')

  and parseArgs tag : scheme_token Stream.t -> var list * int = parser
    | [< 'RPAR >] -> ([], tag)
    | [< 'IDENTIFIER v; (rest, tag') = parseArgs (tag+1) >] ->
      ((v, tag+1) :: rest, tag')

  and parseList tag stream : (scheme_node list) * int = match stream with parser
    | [< 'RPAR >] -> ([], tag)
    | [< (car, tag') = parse' tag; (cdr, tag'') = parseList tag' >] ->
      (car :: cdr, tag'')
  in
  fst (parse' 0 stream)

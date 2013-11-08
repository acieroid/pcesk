open Scheme_tokens
open Scheme_ast

let parse stream =
  let rec parse' tag stream = match stream with parser
    | [< 'IDENTIFIER i >] -> (Identifier i, tag+1)
    | [< 'INTEGER n >] -> (Integer n, tag+1)
    | [< 'STRING s >] -> (String s, tag+1)
    | [< 'BOOLEAN b >] -> (Boolean b, tag+1)
    | [< 'LPAR; 'IDENTIFIER kwd >] -> parseKeyword tag kwd stream

  and parseKeyword tag kwd stream = match kwd with
    | "begin" -> parseBegin tag stream
    | "lambda" -> parseLambda tag stream
    | "define" -> parseDefine tag stream
    | "if" -> parseIf tag stream
    | "set!" -> parseSet tag stream
    | f -> parseFuncall tag (Identifier f, tag+1) stream

  and parseBegin tag = parser
    | [< (body, tag') = parseList (tag+1) >] -> (Begin body, tag+1)

  and parseLambda tag = parser
    | [< 'LPAR; (args, tag') = parseArgs (tag+1);
         (body, tag'') = parseList tag' >] ->
      begin match body with
      | [] -> failwith "Anonymous function without body"
      | _ -> (Lambda (args, body), tag+1)
      end

  and parseDefine tag = parser
    | [< 'LPAR; 'IDENTIFIER n; (args, tag') = parseArgs (tag+2);
         (body, tag'') = parseList tag' >] ->
      begin match body with
      | [] -> failwith ("Function without body: " ^ n)
      | _ -> (DefineFun ((n, tag+2), args, body), tag+1)
      end
    | [< 'IDENTIFIER n; e = parse' (tag+2) >] ->
      (Define ((n, tag+2), e), tag+1)

  and parseIf tag = parser
    | [< (cond, tag') = parse' (tag+1);
         (cons, tag'') = parse' tag';
         (alt, tag''') = parse' tag'' >] ->
      (If ((cond, tag'), (cons, tag''), (alt, tag''')), tag+1)

  and parseSet tag = parser
    | [< 'IDENTIFIER v; e = parse' (tag+2) >] ->
      (Set ((v, tag+2), e), tag+1)

  and parseFuncall tag f = parser
    | [< (args, tag') = parseList (tag+1) >] ->
      (Funcall (f, args), tag+1)

  and parseArgs tag stream = match stream with parser
    | [< 'RPAR >] -> ([], tag)
    | [< 'IDENTIFIER v; (rest, tag') = parseArgs (tag+1) >] ->
      ((v, tag+1) :: rest, tag'+1)

  and parseList tag stream = match stream with parser
    | [< 'RPAR >] -> ([], tag)
    | [< (car, tag') = parse' tag; (cdr, tag'') = parseList tag' >] ->
      ((car, tag') :: cdr, tag'')
  in
  parse' 0 stream

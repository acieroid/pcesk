open Scheme_tokens
open Scheme_ast

let parse stream =
  let rec parse' tag stream = match stream with parser
    | [< 'IDENTIFIER i >] -> ((Identifier i, tag+1), tag+1)
    | [< 'INTEGER n >] -> ((Integer n, tag+1), tag+1)
    | [< 'STRING s >] -> ((String s, tag+1), tag+1)
    | [< 'BOOLEAN b >] -> ((Boolean b, tag+1), tag+1)
    | [< 'LPAR; f = parseFuncall tag >] -> f

  and parseFuncall tag = parser
    | [< 'IDENTIFIER f; k = parseKeyword tag f >] -> k
    | [< (f, tag') = parse' (tag+1); call = parseFuncallRest tag' f >] -> call

  and parseKeyword tag = function
    | "begin" -> parseBegin tag
    | "lambda" -> parseLambda tag
    | "define" -> parseDefine tag
    | "if" -> parseIf tag
    | "set!" -> parseSet tag
    | f -> parseFuncallRest tag ((Identifier f), tag+2)

  and parseBegin tag = parser
    | [< (body, tag') = parseList (tag+1) >] ->
      ((Begin body, tag+1), tag')

  and parseLambda tag = parser
    | [< 'LPAR; (args, tag') = parseArgs (tag+1);
         (body, tag'') = parseList tag' >] ->
      begin match body with
      | [] -> failwith "Anonymous function without body"
      | _ -> ((Lambda (args, body), tag+1), tag'')
      end

  and parseDefine tag = parser
    | [< 'LPAR; 'IDENTIFIER n; (args, tag') = parseArgs (tag+2);
         (body, tag'') = parseList tag' >] ->
      begin match body with
      | [] -> failwith ("Function without body: " ^ n)
      | _ -> ((DefineFun ((n, tag+2), args, body), tag+1), tag'')
      end
    | [< 'IDENTIFIER n; (e, tag') = parse' (tag+2) >] ->
      ((Define ((n, tag+2), e), tag+1), tag')

  and parseIf tag = parser
    | [< (cond, tag') = parse' (tag+1);
         (cons, tag'') = parse' tag';
         (alt, tag''') = parse' tag'' >] ->
      ((If (cond, cons, alt), tag+1), tag''')

  and parseSet tag = parser
    | [< 'IDENTIFIER v; (e, tag') = parse' (tag+2) >] ->
      ((Set ((v, tag+2), e), tag+1), tag')

  and parseFuncallRest tag f = parser
    | [< (args, tag') = parseList (tag+2) >] ->
      ((Funcall (f, args), tag+1), tag')

  and parseArgs tag = parser
    | [< 'RPAR >] -> ([], tag)
    | [< 'IDENTIFIER v; (rest, tag') = parseArgs (tag+1) >] ->
      ((v, tag+1) :: rest, tag')

  and parseList tag = parser
    | [< 'RPAR >] -> ([], tag)
    | [< (car, tag') = parse' tag; (cdr, tag'') = parseList tag' >] ->
      (car :: cdr, tag'')
  in
  fst (parse' 0 stream)

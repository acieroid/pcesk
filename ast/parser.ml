open Tokens
open Ast

let parse stream =
  let rec parse' tag stream = match stream with parser
    | [< 'IDENTIFIER i >] -> ((Identifier i, tag+1), tag+1)
    | [< 'INTEGER n >] -> ((Integer n, tag+1), tag+1)
    | [< 'STRING s >] -> ((String s, tag+1), tag+1)
    | [< 'BOOLEAN b >] -> ((Boolean b, tag+1), tag+1)
    | [< 'NIL >] -> ((Nil, tag+1), tag+1)
    | [< 'LOCKED >] -> ((Locked, tag+1), tag+1)
    | [< 'UNLOCKED >] -> ((Unlocked, tag+1), tag+1)
    | [< 'LPAR; f = parse_funcall tag >] -> f

  and parse_funcall tag = parser
    | [< 'IDENTIFIER f; k = parse_keyword tag f >] -> k
    | [< (f, tag') = parse' (tag+1); call = parse_funcall_rest tag' f >] -> call

  and parse_keyword tag = function
    | "begin" -> parse_begin tag
    | "lambda" -> parse_lambda tag
    | "letrec" -> parse_letrec tag
    | "if" -> parse_if tag
    | "set!" -> parse_set tag
    | "call/cc" -> parse_callcc tag
    | "spawn" -> parse_spawn tag
    | "join" -> parse_join tag
    | "cas" -> parse_cas tag
    | "acquire" -> parse_acquire tag
    | "release" -> parse_release tag
    | f -> parse_funcall_rest tag ((Identifier f), tag+2)

  and parse_begin tag = parser
    | [< (body, tag') = parse_list (tag+1) >] ->
      ((Begin body, tag+1), tag')

  and parse_lambda tag = parser
    | [< 'LPAR; (args, tag') = parse_args (tag+1);
         (body, tag'') = parse_list tag' >] ->
      begin match body with
      | [] -> failwith "Anonymous function without body"
      | _ -> ((Lambda (args, body), tag+1), tag'')
      end

  and parse_letrec tag = parser
    | [< 'LPAR; (bindings, tag') = parse_bindings (tag+1);
         (body, tag'') = parse_list tag' >] ->
      begin match body with
      | [] -> failwith "letrec without body"
      | _ -> ((LetRec (bindings, body), tag+1), tag'')
      end

  and parse_bindings tag = parser
    | [< 'RPAR >] -> ([], tag)
    | [< 'LPAR; 'IDENTIFIER var; (value, tag') = parse' (tag+1); 'RPAR;
         (rest, tag'') = parse_bindings tag' >] ->
      (((var, tag+1), value) :: rest, tag'')

  and parse_if tag = parser
    | [< (cond, tag') = parse' (tag+1);
         (cons, tag'') = parse' tag';
         (alt, tag''') = parse' tag''; 'RPAR >] ->
      ((If (cond, cons, alt), tag+1), tag''')

  and parse_set tag = parser
    | [< 'IDENTIFIER v; (e, tag') = parse' (tag+2); 'RPAR >] ->
      ((Set ((v, tag+2), e), tag+1), tag')

  and parse_callcc tag = parser
    | [< (e, tag') = parse' (tag+1); 'RPAR >] ->
      ((Callcc e, tag+1), tag')

  and parse_spawn tag = parser
    | [< (e, tag') = parse' (tag+1); 'RPAR >] ->
      ((Spawn e, tag+1), tag')

  and parse_join tag = parser
    | [< (e, tag') = parse' (tag+1); 'RPAR >] ->
      ((Join e, tag+1), tag')

  and parse_cas tag = parser
    | [< 'IDENTIFIER v; (e1, tag') = parse' (tag+2);
         (e2, tag'') = parse' tag'; 'RPAR >] ->
      ((Cas ((v, tag+2), e1, e2), tag+1), tag'')

  and parse_acquire tag = parser
    | [< 'IDENTIFIER v; 'RPAR >] ->
      ((Acquire (v, tag+2), tag+1), tag+3)

  and parse_release tag = parser
    | [< 'IDENTIFIER v; 'RPAR >] ->
      ((Release (v, tag+2), tag+1), tag+3)

  and parse_funcall_rest tag f = parser
    | [< (args, tag') = parse_list (tag+2) >] ->
      ((Funcall (f, args), tag+1), tag')

  and parse_args tag = parser
    | [< 'RPAR >] -> ([], tag)
    | [< 'IDENTIFIER v; (rest, tag') = parse_args (tag+1) >] ->
      ((v, tag+1) :: rest, tag')

  and parse_list tag = parser
    | [< 'RPAR >] -> ([], tag)
    | [< (car, tag') = parse' tag; (cdr, tag'') = parse_list tag' >] ->
      (car :: cdr, tag'')
  in
  fst (parse' 0 stream)

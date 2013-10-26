open Scheme_tokens
open Scheme_ast

let rec parse stream = match stream with parser
| [< 'IDENTIFIER i >] -> Identifier i
| [< 'INTEGER n >] -> Integer n
| [< 'STRING s >] -> String s
| [< 'BOOLEAN b >] -> Boolean b
| [< 'QUOTE; exp = parse >] -> List [Identifier "quote"; exp]
| [< 'LPAR; l = parseList >] -> List l

and parseList stream = match stream with parser
| [< 'RPAR >] -> []
| [< car = parse; cdr = parseList >] -> car :: cdr

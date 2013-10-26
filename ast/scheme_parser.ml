open Scheme_tokens
open Scheme_ast

let rec parse stream = match stream with parser
| [< 'IDENTIFIER i >] -> Identifier i
| [< 'INTEGER n >] -> Integer n
| [< 'STRING s >] -> String s
| [< 'BOOLEAN b >] -> Boolean b
| [< 'QUOTE; sym = parse >] -> Pair (Identifier "quote", Pair (sym, Null))
| [< 'LPAR; l = parseList >] -> l

and parseList stream = match stream with parser
| [< 'RPAR >] -> Null
| [< car = parse; cdr = parseList >] -> Pair (car, cdr)

open Types
open Cesk_types

exception NotYetImplemented
exception Malformed of string * Ast.node
exception InvalidKeyword of string
exception UnboundIdentifier of string
exception UnboundAddress of addr
exception InvalidNumberOfArguments of int * int
exception NotAFunction of value
exception NotAKont of value
exception EvaluationStuck of Ast.node
exception PrimWrongArgType of string * value
exception PrimWrongNumberOfArgs of string * int

let string_of_exception = function
  | NotYetImplemented ->
    "Not yet implemented"
  | PrimWrongArgType (s, v) ->
    "Wrong argument type to primitive '" ^ s ^ "': " ^ (string_of_value v)
  | PrimWrongNumberOfArgs (s, n) ->
    "Wrong number of args to primitive '" ^ s ^
      "' (got " ^ (string_of_int n) ^ ")"
  | Malformed (s, n) ->
    "Malformed " ^ s ^ ": " ^ (Ast.string_of_node n)
  | InvalidKeyword k ->
    "Invalid keyword: " ^ k
  | UnboundIdentifier s ->
    "Unbound identifier: " ^ s
  | UnboundAddress a ->
    "Unbound address: " ^ (Addr.string_of_address a)
  | InvalidNumberOfArguments (expected, got) ->
    "Invalid number of arguments: expected " ^ (string_of_int expected) ^
      ", got " ^ (string_of_int got)
  | NotAFunction v ->
    "Not a function: " ^ (string_of_value v)
  | NotAKont v ->
    "Not a continuation: " ^ (string_of_value v)
  | EvaluationStuck n ->
    "Evaluation is stuck at node " ^ (Ast.string_of_node n)
  | e -> raise e

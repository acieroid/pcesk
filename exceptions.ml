open Types

(* Very few exceptions are used since most of the time, when an exception would
 * occur in a normal execution of the program, we just don't create a successor
 * state instead of raising an exception *)

exception NotYetImplemented
exception Malformed of string * Ast.node
exception InvalidKeyword of string
exception UnboundIdentifier of string
exception BadArguments of string
exception UnboundAddress of addr
exception NotAtomic of Ast.node

let string_of_exception = function
  | NotYetImplemented ->
    "Not yet implemented"
  | Malformed (s, n) ->
    "Malformed " ^ s ^ ": " ^ (Ast.string_of_node n)
  | InvalidKeyword k ->
    "Invalid keyword: " ^ k
  | UnboundIdentifier s ->
    "Unbound identifier: " ^ s
  | BadArguments s ->
    "Bad arguments: " ^ s
  | e -> raise e

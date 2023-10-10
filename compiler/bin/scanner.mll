(* Ocamllex scanner for Compost *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

let boolean = "true" | "false" 

let string_char = [^'\'' '\\' ] | "\\\'" | "\\\\"
let string_contents = string_char*

let symlit = '\'' string_contents '\''

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| ';'     { comment lexbuf }            (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ':'      { COLON }
| '\\'     { BACKSLASH }
| '_'      { WILDCARD }
| "if"     { IF }
| "val"    { VAL }
| "define" { DEFINE }
| "datatype" { DATATYPE }
| "use"    { USE }
| "case"   { CASE }
| "begin"  { BEGIN }
| "let"    { LET }
| "int"    { INT }
| "bool"   { BOOL }
| "sym"    { SYM }
| digits as lxm { INTLIT(int_of_string lxm) }
| boolean as lxm { BOOLLIT(bool_of_string lxm) }
| symlit as lxm { SYMLIT(String.sub lxm 1 (String.length lxm - 2)) } (* Trim out quote characters *)
| [^'(' ')' '[' ']' '\'' ' ' ';']+ as lxm { NAME(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }


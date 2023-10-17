(* Ocamllex scanner for Compost *)

{
  open Parser

  let format_sym_lit sym_lit =
  let sym_len = String.length sym_lit in
  let sym_body = Str.string_after (Str.string_before sym_lit (sym_len - 1)) 1 in
  let escape_backslash = Str.global_replace (Str.regexp {|\\\\|}) "\\\\" in
  let escape_single_quote = Str.global_replace (Str.regexp {|\\'|}) "'" in
  escape_backslash (escape_single_quote sym_body)
}

let digit = ['0' - '9']
let digits = ('+' | '-' | "") digit+

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
| '_'      { WILDCARD }
| "->"     { ARROW }
| "if"     { IF }
| "val"    { VAL }
| "define" { DEFINE }
| "datatype" { DATATYPE }
| "use"    { USE }
| "case"   { CASE }
| "begin"  { BEGIN }
| "let"    { LET }
| "dup"    { DUP }
| "int"    { INT }
| "bool"   { BOOL }
| "sym"    { SYM }
| "unit"   { UNIT }
| digits as lxm { INTLIT(int_of_string lxm) }
| boolean as lxm { BOOLLIT(bool_of_string lxm) }
| symlit as lxm { SYMLIT(format_sym_lit lxm) }
| [^'(' ')' '[' ']' '\'' ' ' '\t' '\r' '\n' ';']+ as lxm { NAME(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }


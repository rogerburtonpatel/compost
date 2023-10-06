(* Ocamllex scanner for Compost *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

let boolean = "true" | "false" 

let string_char = [ ^ '\'' '\\' ] | "\\'"
let string_contents = string_char+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| ';'     { comment lexbuf }            (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ':'      { COLON }
| '\''     { symbol_start lexbuf }
| '\\'     { BACKSLASH }
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
| [^'(' ')' '[' ']' '\'' ' ']+ as lxm { NAME(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }

(* NEEDSWORK: Symbol parsing is nonfunctional. Also needs to be changed to 
 * allow empty strings *)
and symbol_start = parse 
  string_contents as lxm { SYMLIT(lxm) }
| '\'' { token lexbuf }
| _ { raise (Failure("invalid symbol")) }


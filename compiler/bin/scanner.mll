(* Ocamllex scanner for Compost *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

let boolean = "true" | "false" 

let string_contents = "(?:[^'\\]|\\.)*"

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| ';'     { comment lexbuf }            (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ':'      { COLON }
| '\''     { symbol lexbuf }
| '\\'     { BACKSLASH }
| "if"     { IF }
| "val"    { VAL }
| "define" { DEFINE }
| "datatype" { DATATYPE }
| "use"    { USE }
| "case"   { CASE }
| "begin"  { BEGIN }
| "let"    { LET }
| "int"    { INTTYPE }
| "bool"   { BOOLTYPE }
| "sym"    { SYMTYPE }
| digits as lxm { INTLIT(int_of_string lxm) }
| boolean as lxm { BOOLLIT(bool_of_string lxm) }
| [^'(' ')' '[' ']' '\'' ' ']+ as lxm { NAME(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }

and symbol = parse 
| '\'' { token lexbuf }
| string_contents as lxm { SYM(lxm) }
| _ { raise (Failure("invalid string")) }

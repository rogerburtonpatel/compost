/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token LPAREN RPAREN
%token EOF

%start program
%type <Ast.program> program

/* Associativity and precedence of tokens encoded here */

%%

program:
  defs EOF { $1 }

defs:
 | defs /* combo of tokens and nonterminals */ { (* ML code using $1, $2, $3... *) }


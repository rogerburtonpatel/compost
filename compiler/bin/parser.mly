/* Ocamlyacc parser for MicroC */

%{
  open Ast
%}

%token LPAREN RPAREN LBRACKET RBRACKET 
%token QUOTE ESCAPE BACKSLASH 
%token VAL DEFINE DATATYPE USE COLON 
%token CASE IF BEGIN LET 
%token INTTYPE BOOLTYPE UNITTYPE SYMTYPE 
%token EOF 

%token <string> NAME
%token <int> INTLIT
%token <bool> BOOLLIT
%token 

%start program
%type <Ast.program> program

/* Associativity and precedence of tokens encoded here */

%%

program:
  defs EOF { $1 }

defs:
 | defs /* combo of tokens and nonterminals */ { (* ML code using $1, $2, $3... *) }


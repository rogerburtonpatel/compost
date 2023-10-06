/* Ocamlyacc parser for MicroC */

%{
  open Ast
%}

%token LPAREN RPAREN LBRACKET RBRACKET 
%token QUOTE ESCAPE BACKSLASH 
%token VAL DEFINE DATATYPE USE COLON 
%token CASE IF BEGIN LET 
%token INT BOOL UNIT SYM 
%token EOF 

%token <string> NAME SYMLIT 
%token <int> INTLIT
%token <bool> BOOLLIT
%token UNITLIT

%start program
%type <Ast.program> program

/* Associativity and precedence of tokens encoded here */

%%

/* NEEDSWORK: Change to ensure that LPAREN cannot be matched with RBRACKET */
lbracket: LPAREN | LBRACKET { }

rbracket: RPAREN | RBRACKET { }

program:
  defs EOF { $1 }

defs:
   /* nothing */ { [] }
 | def defs { $1 :: $2 }

def:
   lbracket VAL NAME expr rbracket { Val($3, $4) }
 | lbracket DEFINE NAME lbracket namelist rbracket expr rbracket { Define($3, $5, $7) }
   /* NEEDSWORK: Implement rest of defines */

namelist:
   /* nothing */  { [] }
 | NAME namelist { $1 :: $2 }

expr:
   literal { Literal($1) }
 | NAME { NameExpr($1) } 
 | lbracket IF expr expr expr rbracket { If($3, $4, $5) }
 | lbracket BEGIN exprlist rbracket { Begin($3)}
 | lbracket exprlist rbracket { Block($2) }
 | lbracket LET lbracket bindlist rbracket expr rbracket { Let($4, $6) }
   /* NEEDSWORK: Implement case */

exprlist:
   /* nothing */ { [] }
 | expr exprlist { $1 :: $2 }

bindlist:
   /* nothing */ { [] }
 | lbracket NAME expr rbracket bindlist { ($2, $3) :: $5 }

literal:
   INTLIT { IntLit($1) }
 | SYMLIT { SymLit($1) }
 | BOOLLIT { BoolLit($1) }
 | UNITLIT { UnitLit }


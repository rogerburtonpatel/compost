/* Ocamlyacc parser for MicroC */

%{
  open Ast
  open Freshnames
%}

%token LPAREN RPAREN LBRACKET RBRACKET 
%token VAL DEFINE DATATYPE USE COLON 
%token CASE IF BEGIN LET 
%token INT BOOL SYM UNIT 
%token WILDCARD ARROW DUP 
%token EOF 

%token <string> NAME SYMLIT 
%token <int> INTLIT
%token <bool> BOOLLIT

%start program
%type <Ast.program> program

/* No need to specify any associativity or precedence in our syntax because 
   our parenthesized syntax makes everything explicit
 */

%%

program:
  defs EOF { $1 }

varname:
   NAME { $1 }
 | INT { "int" }
 | BOOL { "bool" }
 | SYM { "sym" }
 | ARROW { "->" }

tyname:
   NAME { $1 }

defs:
   /* nothing */ { [] }
 | def defs { $1 :: $2 }

def:
   LPAREN definternal RPAREN { $2 }
 | LBRACKET definternal RBRACKET { $2 }

definternal:
   VAL varname expr { Val($2, $3) }
 | DEFINE varname parenvarnamelist expr { Define($2, $3, $4) }
 | DATATYPE tyname parenvariantlist { Datatype($2, $3) }
 | COLON varname ty { TyAnnotation($2, $3) }
 | USE filename { Use($2) }

parenvarnamelist:
   LPAREN varnamelist RPAREN { $2 }
 | LBRACKET varnamelist RBRACKET { $2 }

varnamelist:
   /* nothing */  { [] }
 | varname varnamelist { $1 :: $2 }

parenvariantlist:
   LPAREN variantlist RPAREN { $2 }
 | LBRACKET variantlist RBRACKET { $2 }

variantlist:
   /* nothing */ { [] }
 | variant variantlist { $1 :: $2 }

variant: 
   LPAREN variantinternal RPAREN { $2 }
 | LBRACKET variantinternal RBRACKET { $2 }

variantinternal: varname parentylist { ($1, $2) }

parentylist:
   LPAREN tylist RPAREN { $2 }
 | LBRACKET tylist RBRACKET { $2 }

tylist:
   /* nothing */ { [] }
 | ty tylist { $1 :: $2 }

ty: 
   LPAREN funtyinternal RPAREN { $2 }
 | LBRACKET funtyinternal RBRACKET { $2 }
 | INT { Int }
 | BOOL { Bool }
 | SYM { Sym }
 | UNIT { Unit }
 | tyname { CustomTy($1) }

funtyinternal:
   ARROW parentylist ty { FunTy($2, $3) }

filename: SYMLIT { $1 } 

expr:
   literal { Literal($1) }
 | varname { NameExpr($1) } 
 | LPAREN exprinternal RPAREN { $2 }
 | LBRACKET exprinternal RBRACKET { $2 }

exprinternal:
   IF expr expr expr { If($2, $3, $4) }
 | BEGIN exprlist { Begin($2)}
 | expr exprlist { Apply($1, $2) }
 | LET parenbindlist expr { Let($2, $3) }
 | CASE expr parencasebranchlist { Case($2, $3) }
 | DUP varname { Dup($2) }

exprlist:
   /* nothing */ { [] }
 | expr exprlist { $1 :: $2 }

parenbindlist:
   LPAREN bindlist RPAREN { $2 }
 | LBRACKET bindlist RBRACKET { $2 }

bindlist:
   /* nothing */ { [] }
 | LPAREN bind RPAREN bindlist { $2 :: $4 }
 | LBRACKET bind RBRACKET bindlist { $2 :: $4 }

bind: varname expr { ($1, $2) }

casebranch: 
   LPAREN pattern expr RPAREN { ($2, $3) }
 | LBRACKET pattern expr RBRACKET { ($2, $3) }

parencasebranchlist:
   LPAREN casebranchlist RPAREN { $2 }
 | LBRACKET casebranchlist RBRACKET { $2 }

casebranchlist:
   /* nothing */ { [] }
 | casebranch casebranchlist { $1 :: $2 }

pattern:
   LPAREN patterninternal RPAREN { $2 }
 | LBRACKET patterninternal RBRACKET { $2 }
 | WILDCARD { WildcardPattern }
 | varname { Name $1 }
//  TODO TEST THIS ^ 

patterninternal: varname nameorwildcardlist { Pattern($1, $2) }

nameorwildcardlist:
   /* nothing */ { [] }
 | nameorwildcard nameorwildcardlist { $1 :: $2 }

nameorwildcard:
   varname { $1 }
 | WILDCARD { fresh_name () }

literal:
   INTLIT { IntLit($1) }
 | SYMLIT { SymLit($1) }
 | BOOLLIT { BoolLit($1) }
 | UNIT { UnitLit }


/* Ocamlyacc parser for MicroC */

%{
  open Ast
%}

%token LPAREN RPAREN LBRACKET RBRACKET 
%token VAL DEFINE DATATYPE USE COLON 
%token CASE IF BEGIN LET 
%token INT BOOL SYM 
%token WILDCARD ARROW DUP 
%token EOF 

%token <string> NAME SYMLIT 
%token <int> INTLIT
%token <bool> BOOLLIT
%token UNITLIT

%start program
%type <Ast.program> program

/* No need to specify any associativity or precedence in our syntax because 
   our parenthesized syntax makes everything explicit
 */

%%

program:
  defs EOF { $1 }

defs:
   /* nothing */ { [] }
 | def defs { $1 :: $2 }

def:
   LPAREN definternal RPAREN { $2 }
 | LBRACKET definternal RBRACKET { $2 }

definternal:
   VAL NAME expr { Val($2, $3) }
 | DEFINE NAME parennamelist expr { Define($2, $3, $4) }
 | DATATYPE NAME parenvariantlist { Datatype($2, $3) }
 | COLON NAME ty { TyAnnotation($2, $3) }
 | USE filename { Use($2) }

parennamelist:
   LPAREN namelist RPAREN { $2 }
 | LBRACKET namelist RBRACKET { $2 }

namelist:
   /* nothing */  { [] }
 | NAME namelist { $1 :: $2 }

parenvariantlist:
   LPAREN variantlist RPAREN { $2 }
 | LBRACKET variantlist RBRACKET { $2 }

variantlist:
   /* nothing */ { [] }
 | variant variantlist { $1 :: $2 }

variant: 
   LPAREN variantinternal RPAREN { $2 }
 | LBRACKET variantinternal RBRACKET { $2 }

variantinternal: NAME parentylist { Variant($1, $2) }

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
 | NAME { CustomTy($1) }

funtyinternal:
   ARROW parentylist ty { FunTy($2, $3) }

/* NEEDSWORK: Decide if we want more specific restrictions on filenames */
filename: NAME { $1 } 

expr:
   literal { Literal($1) }
 | NAME { NameExpr($1) } 
 | LPAREN exprinternal RPAREN { $2 }
 | LBRACKET exprinternal RBRACKET { $2 }

exprinternal:
   IF expr expr expr { If($2, $3, $4) }
 | BEGIN exprlist { Begin($2)}
 | expr exprlist { Apply($1, $2) }
 | LET parenbindlist expr { Let($2, $3) }
 | CASE expr parencasebranchlist { Case($2, $3) }
 | DUP NAME { Dup($2) }

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

bind: NAME expr { ($1, $2) }

casebranch: 
   LPAREN pattern expr RPAREN { CaseBranch($2, $3) }
 | LBRACKET pattern expr RBRACKET { CaseBranch($2, $3) }

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

patterninternal: NAME nameorwildcardlist { Pattern($1, $2) }

nameorwildcardlist:
   /* nothing */ { [] }
 | nameorwildcard nameorwildcardlist { $1 :: $2 }

nameorwildcard:
   NAME { PatternBindVar($1) }
 | WILDCARD { WildcardBind }

literal:
   INTLIT { IntLit($1) }
 | SYMLIT { SymLit($1) }
 | BOOLLIT { BoolLit($1) }
 | UNITLIT { UnitLit }


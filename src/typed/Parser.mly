%{

open Syntax

%}

%token LPAREN RPAREN LANGLE RANGLE COLON COMMA EQUALS DOT
%token PLUS MINUS STAR RARROW EMPTYQ
%token COLONCOLON
%token INT_TOKEN BOOL LIST IF THEN ELSE LET IN EMPTY HEAD TAIL TRUE FALSE FUN FIX
%token EOF
%token<string> ID
%token<int> INT

%start program
%type <Syntax.exp> program

%%

list_typ :
  | INT_TOKEN { TInt }
  | BOOL { TBool }
  | list_typ LIST { TList $1 }
  | LPAREN typ RPAREN { $2 }

fn_typ :
  | list_typ { $1 }
  | list_typ RARROW fn_typ { TFun ($1, $3) }

typ :
  | fn_typ { $1 }


atom :
  | INT { Const (Int $1) }
  | ID { Id $1 }
  | TRUE { Const (Bool true) }
  | FALSE { Const (Bool false) }
  | EMPTY LANGLE typ RANGLE { Empty $3 }
  | LPAREN exp RPAREN { $2 }

app :
  | atom { $1 }
  | HEAD atom { Head $2 }
  | TAIL atom { Tail $2 }
  | EMPTYQ atom { IsEmpty $2 }
  | app atom { App ($1, $2) }

list_ :
  | app { $1 }
  | app COLONCOLON list_ { Cons ($1, $3) }

mul :
  | list_ { $1 }
  | mul STAR list_ { Op2 (Mul, $1, $3) }

add :
  | mul { $1 }
  | add MINUS mul { Op2 (Sub, $1, $3) }
  | add PLUS mul { Op2 (Add, $1, $3) }

cmp :
  | add { $1 }
  | add EQUALS add { Op2 (Eq, $1, $3) }
  | add LANGLE add { Op2 (LT, $1, $3) }
  | add RANGLE add { Op2 (GT, $1, $3) }

exp :
  | cmp { $1 }
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
  | LET ID EQUALS exp IN exp { Let ($2, $4, $6) }
  | FUN LPAREN ID COLON typ RPAREN RARROW exp { Fun ($3, $5, $8) }
  | FIX LPAREN ID COLON typ RPAREN RARROW exp { Fix ($3, $5, $8) }

program :
  | exp EOF { $1 }
%%
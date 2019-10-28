%{
  open Ast
%}

%token LB RB
%token CMA SCN
%token ABORT SKIP
%token Q
%token <int> ID
%token EOF

%start program
%type <Ast.program> program

%%

program :
| ops EOF { $1 }
;

ops :
| op { [ $1 ] } 
| op SCN ops { $1 :: $3 }
;

op :
| ABORT LB qbs RB { Abort $3 }
| SKIP LB qbs RB { Skip $3 }
;

qbs :
| qb { [ $1 ] }
| qb CMA qbs { $1 :: $3 }
;

qb :
| Q ID { Q $2 }
;


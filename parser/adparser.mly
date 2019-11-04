%{
  open Ast
%}

%token LB RB
%token LBC RBC
%token LBS RBS
%token CMA SCN
%token ABORT SKIP
%token Q
%token PARA
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
| ABORT LBC parsl RBC LB qbs RB { Abort ($3, $6) }
| SKIP LBC parsl RBC LB qbs RB { Skip ($3, $6) }
;

qbs :
| qb { [ $1 ] }
| qb CMA qbs { $1 :: $3 }
;

qb :
| Q ID { Q $2 }
;


parsl :
| pars { [ $1 ] }
| pars CMA parsl { $1 :: $3 }
;

pars :
| PARA ID { PARA $2 }
;



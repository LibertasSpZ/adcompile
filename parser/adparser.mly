%{
  open Ast
%}

%token LB RB
%token LBC RBC
%token LBS RBS
%token CMA SCN
%token ABORT SKIP
%token Q T
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
| ABORT pars qbs { Abort ($2, $3) }
| SKIP pars qbs { Skip ($2, $3) }
;

qbs :
| LB qbl RB { $2 }

qbl :
| qb { [ $1 ] }
| qb CMA qbl { $1 :: $3 }
;

qb :
| Q ID { Q $2 }
;

pars :
| LBC RBC { [] }
| LBC parl RBC { $2 }

parl :
| par { [ $1 ] }
| par CMA parl { $1 :: $3 }
;

par :
| T ID { T $2 }
;



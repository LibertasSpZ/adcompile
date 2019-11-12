%{
  open Ast
%}

%token LB RB
%token LBC RBC
%token LBS RBS
%token CMA SCN
%token ABORT SKIP ASS EQS KETL KETR
%token H CN X Y Z XX YY ZZ CX CY CZ CXX CYY CZZ
%token MEAS CASE THEN ELSE FI WHILE DO OD

%token Q T
%token <int> ID

%token EOF

%start program
%type <Ast.program> program

%%

program :
| op EOF { $1 }
;




op :
| ABORT pars qbs { Abort ($2, $3) }
| SKIP pars qbs { Skip ($2, $3) }
| qb ASS KETL ID KETR { Init $1 }
| paredu qbs { Uapp ($1, $2)}
| op SCN op { Seq ($1, $3) }
| CASE MEAS qbs EQS ID THEN op ELSE op FI { Case ($3, $7, $9) }
| WHILE LBS ID RBS MEAS qbs EQS ID DO op OD { Bwhile ($3, $6, $10)} 
;

paredu :
| H pars { H $2 }
| CN pars { CN $2 }
| X pars { X $2 }
| Y pars { Y $2 }
| Z pars { Z $2 }
| XX pars { XX $2 }
| YY pars { YY $2 }
| ZZ pars { ZZ $2 }
| CX pars { CX $2 }
| CY pars { CY $2 }
| CZ pars { CZZ $2 }
| CXX pars { CXX $2 }
| CYY pars { CYY $2 }
| CZZ pars { CZZ $2 }
;

qbs :
| LB qbl RB { $2 }
;
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



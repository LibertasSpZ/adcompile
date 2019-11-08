
type id = int
type parid = int

type qb = 
| Q of id

type par = 
| T of parid



type qbl = qb list
type parl = par list

type unitary =
| H of parl
| CN of parl
| X of parl
| Y of parl
| Z of parl
| XX of parl
| YY of parl
| ZZ of parl
| CX of parl
| CY of parl
| CZ of parl
| CXX of parl
| CYY of parl
| CZZ of parl



type op = 
| Abort of parl * qbl
| Skip of parl * qbl
| Init of qb 
| Uapp of unitary * qbl
| Seq of op * op 
| Case of qbl * op * op
| Bwhile of id * qbl * op 


type program = op (* list *)

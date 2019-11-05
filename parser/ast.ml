
type id = int
type parid = int

type qb = 
| Q of id

type par = 
| T of parid

type qbl = qb list
type parl = par list

type op = 
| Abort of parl * qbl
| Skip of parl * qbl

type program = op list

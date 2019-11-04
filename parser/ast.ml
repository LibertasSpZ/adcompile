
type id = int
type parid = int

type qb = 
| Q of id

type pars = 
| PARA of parid

type qbl = qb list
type parsl = pars list

type op = 
| Abort of parsl * qbl
| Skip of parsl * qbl

type program = op list

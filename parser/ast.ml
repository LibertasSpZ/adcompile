
type id = int

type qb = 
| Q of id

type qbl = qb list

type op = 
| Abort of qbl
| Skip of qbl

type program = op list

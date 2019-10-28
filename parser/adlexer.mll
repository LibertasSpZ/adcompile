{
  open Adparser
}

rule token = parse
| [ ' ' '\t' '\n' ] { token lexbuf } (* escape spaces *)
| "[" { LB }
| "]" { RB }
| "," { CMA }
| ";" { SCN }
| "abort" { ABORT }
| "skip" { SKIP }
| "q" { Q }
| ['0'-'9']['0'-'9']* as str { ID(int_of_string(str)) }
| eof { EOF }
| _ as chr { failwith ("lex error: "^(Char.escaped chr))}
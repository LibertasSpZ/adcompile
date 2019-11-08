{
  open Adparser
}

rule token = parse
| [ ' ' '\t' '\n' ] { token lexbuf } (* escape spaces *)
| "[" { LB }
| "]" { RB }
| "(" { LBC }
| ")" { RBC }
| "{" { LBS }
| "}" { RBS }
| ":=" { ASS }
| "|" { KETL }
| ">" { KETR }
| "," { CMA }
| ";" { SCN }
| "abort" { ABORT }
| "skip" { SKIP }
| "H" { H }
| "CNOT" { CN }
| "X" { X }
| "Y" { Y }
| "Z" { Z }
| "XX" { XX }
| "YY" { YY }
| "ZZ" { ZZ }
| "C-X" { CX }
| "C-Y" { CY }
| "C-Z" { CZ }
| "C-XX" { CXX }
| "C-YY" { CYY }
| "C-ZZ" { CZZ }
| "q" { Q }
| "t" { T }
| ['0'-'9']['0'-'9']* as str { ID(int_of_string(str)) }
| eof { EOF }
| _ as chr { failwith ("lex error: "^(Char.escaped chr))}
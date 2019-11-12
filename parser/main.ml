
open Printf 
open Ast
open Compiler


(* Needs a parser-> compiler mapping function, and a driver to
execute the conversion. Worry about printing later! *)
let astqubit_to_str qub = 
	match qub with
| Ast.Q num -> let snum = string_of_int(num) in 
			   let scum = "q" ^ snum in 
			   Compiler.Qvar scum

let rec astqubl_to_strl qubl = 
	match qubl with
| [] -> []	
| (Ast.Q num) :: qubl2 -> (astqubit_to_str (Ast.Q num)) :: (astqubl_to_strl qubl2)
(* Need to convert the Parser qubit lists to compiler qubit 
lists too. *)

(* let rec str_to_complql strl =
	match strl with
| [] -> []
| x :: l' -> (Compiler.Qvar x) :: (str_to_complql l') *)

let rec parser_to_compiler prog =
	match prog with
| Ast.Abort (a,b) -> Compiler.Abort ((* str_to_complql*)(astqubl_to_strl b)) 
| _ -> Compiler.Abort [Compiler.Qvar "bj"]


(*** The above: mapping btw parser to compler ***)



let parse_file f  =
  let lexbuf = Lexing.from_channel (open_in f) in
  Adparser.program Adlexer.token lexbuf


let automate_qiuDao f papar = 
	let prog = parse_file f in 
	let c4 = parser_to_compiler (prog) in
	let ndprog = normalToNonDet (c4) in 
	(* let papar = "t2" in *)
	let daoChengXu = codeTransformation ndprog papar in 
  (* let opDaoChengXU = *) codeCompilation daoChengXu

(*** let test_p10 = 
let c4 = parser_to_compiler (parse_file) in
let ndprog = normalToNonDet (c4) in 
let papar = "t2" in 
let daoChengXu = codeTransformation ndprog papar in 
  (* let opDaoChengXU = *) codeCompilation daoChengXu  ***)

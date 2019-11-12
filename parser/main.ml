
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


let astpar_to_str parb = 
	match parb with
| Ast.T num -> let snum = string_of_int(num) in 
			   "t" ^ snum


let rec astqubl_to_strl qubl = 
	match qubl with
| [] -> []	
| (Ast.Q num) :: qubl2 -> (astqubit_to_str (Ast.Q num)) :: (astqubl_to_strl qubl2)

let rec astparl_to_strl parl = 
	match parl with
| [] -> []	
| (Ast.T num) :: parl2 -> (astpar_to_str (Ast.T num)) :: (astparl_to_strl parl2)



let astU_to_compU unita = 
	match unita with 
| Ast.H parl -> Compiler.Gate("H",(astparl_to_strl parl))
| Ast.CN parl -> Compiler.Gate("CNOT",(astparl_to_strl parl))
| Ast.CX parl -> Compiler.Gate("C-X",(astparl_to_strl parl))
| Ast.CY parl -> Compiler.Gate("C-Y",(astparl_to_strl parl))
| Ast.CZ parl -> Compiler.Gate("C-Z",(astparl_to_strl parl))
| Ast.CXX parl -> Compiler.Gate("C-XX",(astparl_to_strl parl))
| Ast.CYY parl -> Compiler.Gate("C-YY",(astparl_to_strl parl))
| Ast.CZZ parl -> Compiler.Gate("C-ZZ",(astparl_to_strl parl))
| Ast.X parl -> Compiler.OneBRot("X",(astparl_to_strl parl))
| Ast.Y parl -> Compiler.OneBRot("Y",(astparl_to_strl parl))
| Ast.Z parl -> Compiler.OneBRot("Z",(astparl_to_strl parl))
| Ast.XX parl -> Compiler.TwoBRot("XX",(astparl_to_strl parl))
| Ast.YY parl -> Compiler.TwoBRot("YY",(astparl_to_strl parl))
| Ast.ZZ parl -> Compiler.TwoBRot("ZZ",(astparl_to_strl parl))





(* Need to convert the Parser qubit lists to compiler qubit 
lists too. *)

(* let rec str_to_complql strl =
	match strl with
| [] -> []
| x :: l' -> (Compiler.Qvar x) :: (str_to_complql l') *)

let rec parser_to_compiler prog =
	match prog with
| Ast.Abort (a,b) -> Compiler.Abort ((* str_to_complql*)(astqubl_to_strl b)) 
| Ast.Skip (a,b) -> Compiler.Skip ((* str_to_complql*)(astqubl_to_strl b)) 
| Ast.Uapp (a,b) -> Compiler.Uapp ((astU_to_compU a), (astqubl_to_strl b))
| Ast.Init q -> Compiler.Init (astqubit_to_str q)
| Ast.Seq (a,b) -> Compiler.Seq ((parser_to_compiler a), (parser_to_compiler b))
| Ast.Case (ql, p1, p2) -> Compiler.Case (astqubit_to_str (List.hd ql), parser_to_compiler p1,
							parser_to_compiler p2)
| Ast.Bwhile (n, ql, p1) -> Compiler.Bwhile (n, astqubit_to_str (List.hd ql), 
							parser_to_compiler p1)
(* | _ -> Compiler.Abort [Compiler.Qvar "WA"] *)


(*** The above: mapping btw parser to compler ***)



let parse_file f  =
  let lexbuf = Lexing.from_channel (open_in f) in
  Adparser.program Adlexer.token lexbuf


let rec printList li =
 match li with 
 | [] -> let cnextline = "\n End of printing \n" in 
                 print_endline cnextline
 | x :: l' -> let cnextline = "\n" in 
             let s6 = unparse_com x in
             print_endline s6;
             print_endline cnextline;
             printList l'

let automate_qiuDao f papar = 
	let prog = parse_file f in 
	let c4 = parser_to_compiler (prog) in
	let ndprog = normalToNonDet (c4) in 
	(* let papar = "t2" in *)
	let daoChengXu = codeTransformation ndprog papar in 
  (* let opDaoChengXU = *) printList (codeCompilation daoChengXu)

let () = 
  let len = Array.length Sys.argv in
    match len with
    | 3 -> automate_qiuDao Sys.argv.(1) Sys.argv.(2)
    | _ -> print_endline "error!"
 
(*** let test_p10 = 
let c4 = parser_to_compiler (parse_file) in
let ndprog = normalToNonDet (c4) in 
let papar = "t2" in 
let daoChengXu = codeTransformation ndprog papar in 
  (* let opDaoChengXU = *) codeCompilation daoChengXu  ***)

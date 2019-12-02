
open Printf 
open Ast
open Compiler

(***  Below are a few helper functions to connect the parsed progs 
    (Ast.) to the programs for compiler (Compiler.) 

  ***)

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







let rec parser_to_compiler prog =
	match prog with
| Ast.Abort (a,b) -> Compiler.Abort ((astqubl_to_strl b)) 
| Ast.Skip (a,b) -> Compiler.Skip ((astqubl_to_strl b)) 
| Ast.Uapp (a,b) -> Compiler.Uapp ((astU_to_compU a), (astqubl_to_strl b))
| Ast.Init q -> Compiler.Init (astqubit_to_str q)
| Ast.Seq (a,b) -> Compiler.Seq ((parser_to_compiler a), (parser_to_compiler b))
| Ast.Case (ql, p1, p2) -> Compiler.Case (astqubit_to_str (List.hd ql), parser_to_compiler p1,
							parser_to_compiler p2)
| Ast.Bwhile (n, ql, p1) -> Compiler.Bwhile (n, astqubit_to_str (List.hd ql), 
							parser_to_compiler p1)


(*** The above: mapping btw parser to compler ***)





(*** Read in the file ***)

let parse_file f  =
  let lexbuf = Lexing.from_channel (open_in f) in
  Adparser.program Adlexer.token lexbuf


(*** Helper function for printing out the elements in the set of Compile(P) 
one by one.
***)

let rec printList li =
 match li with 
 | [] -> let cnextline = "\n End of listing programs in the compiled set! \n" in 
                 print_endline cnextline
 | x :: l' -> let cnextline = "\n" in 
             let s6 = unparse_com x 0 in
             print_endline s6;
             print_endline cnextline;
             printList l'



(***  automate_qiuDao is a driver for code-transformation.  ***)


let automate_qiuDao f papar = 
	let prog = parse_file f in 
	let c4 = parser_to_compiler (prog) in
	let ndprog = normalToNonDet (c4) in 
	codeTransformation ndprog papar 



(*** Below: Final driver, calls automate_qiuDao to print the code transformation
result, then applies compilation rules to obtain the compiled set of programs 
Compile(P), and finally prints out the list.  ***)



let () = 
  let len = Array.length Sys.argv in
    match len with
    | 3 -> let cnextline = "\n # of progs in the compiled multiset of derivative prog: \n" in
    	   let cnextline2 = "\n Thanks for using our product. Have a good day. \n" in
    	   let cnextline3 = "\n The transformed code (as Summation of progs) is: \n " in 
    	   let cnextline4 = "\n and the elements of the complied multiset of derivative progs are: \n" in 
    	   let  daoChengXu = automate_qiuDao Sys.argv.(1) Sys.argv.(2) in 
    	   let ctDaochengdu = unparse_UnderlineCom daoChengXu 0 in 
    	   let resli = codeCompilation daoChengXu in 
    	   let bd = List.length(resli) in
    	   	   print_endline cnextline3;
    	   	   print_endline ctDaochengdu;
    	   	   print_endline cnextline4;
    	       printList resli; 
    	       print_endline cnextline;
    	       Printf.printf " %d\n" bd;  
    	      
    	       print_endline cnextline2 
    | _ -> print_endline "error! Please pass me exactly 2 args, first the file, second the parameter!
    Have a good day. "
 

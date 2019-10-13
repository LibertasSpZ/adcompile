type qid = string
type uid = string
type parid = string
(* theta1, theta2 etc  *)
type par = parid list
(* The list of parameters  *)

type qbit =
| Qvar of qid

type qlist = qbit list

type unitary = 
| Gate of uid * par

type com =
| Abort of qlist
| Skip of qlist
| Init of qbit
| Uapp of unitary * qlist
| Seq of com * com
| Case of qbit * com * com
| Bwhile of int * qbit * com 

let unparse_qbit qb : string =
  match qb with
  | Qvar q -> q


let rec unparse_qlist ql : string =
  match ql with
  | [] -> ""
  | q :: [] -> unparse_qbit q
  | q :: l -> 
     let uq = unparse_qbit q in
     let ul = unparse_qlist l in
     uq ^ "," ^ ul




let rec unparse_par theta : string =
   match theta with
   | [] -> ""
   | p :: [] -> p
   | p :: l ->  
      let sl = unparse_par l in
      p ^ "," ^ sl


let unparse_unitary u : string =
  match u with
  | Gate (g, t) -> 
     let st = unparse_par t in 
     g ^ "(" ^ st ^ ")"

let rec unparse_com c : string = 
  match c with
  | Abort ql -> 
     let sql = unparse_qlist ql in
     "Abort[" ^ sql ^ "]"
  | Skip ql -> 
     let sql = unparse_qlist ql in
      "Skip[" ^ sql ^ "]"
  | Init q -> 
     let s = unparse_qbit q in 
     s ^ ":=∣0⟩"
  | Uapp (u, ql) -> 
     let su = unparse_unitary u in 
     let sql = unparse_qlist ql in 
     sql ^ ":=" ^ su ^ "[" ^ sql ^ "]"
  | Seq (c1, c2) ->
     let s1 = unparse_com c1 in
     let s2 = unparse_com c2 in
     s1 ^ "; " ^ s2
  | Case (qb, u1, u2) ->
     let q = unparse_qbit qb in
     let s1 = unparse_com u1 in
     let s2 = unparse_com u2 in
     "case M(" ^ q ^ ") = 0 then " ^ s1 ^ "else" ^ s2
  | Bwhile (num, qb, u1) ->
     let nt = Printf.sprintf "%d" num in
     let q = unparse_qbit qb in
     let s1 = unparse_com u1 in
     "while^" ^ nt ^ " M(" ^ q ^ ")= 1 do " ^ s1   

let test_p1 =
  let c1 = Init (Qvar "q0") in
  let c2 = (Uapp (Gate ("H", ["theta"]), [Qvar "q0"])) in
  Seq(Seq(c1, c2), c2)

let test_p2 =
  let c0 = Init (Qvar "q0") in
  let c1 = Init (Qvar "q1") in
  let c2 = (Uapp (Gate ("H", ["theta_1"]), [Qvar "q0"])) in
  let c3 = (Uapp (Gate ("CNOT", ["theta_2"]), [Qvar "q0"; Qvar "q1"])) in
  let c4 = Seq(Seq(c0, c1), Seq(c2, c3)) in
  let guard = Qvar "q3" in 
  Bwhile (2, guard, c4)     
 
(* Changed 0.5 and 0.3 to theta_1 and theta_2 *)
  
let () =
  let c = test_p2 in
  (* let c = Skip in  *)
  let s = unparse_com c in
  print_endline s


  (* TODO: We need to be assign values to parameters,
     so need a function type assign: parid -> float;

     We also need to define a function type mapping from parameterized
     unitary to actual unitaries, i.e.
     a function type that takes a (parameterized)unitary * assign, returns
     unparameterized unitary. This function assigns
     real values to the parameters in the parameterized unitary,
     and outputs an unparameterized unitary. The assignment is done
     in compliance with the "assign" function *)

type qid = string
type uid = string
type par = float list

type qbit =
| Qvar of qid

type qlist = qbit list

type unitary = 
| Gate of uid * par

type com =
| Abort of qlist
| Skip
| Init of qbit
| Uapp of unitary * qlist
| Seq of com * com
(* | Case of qbit * com * com
 * | Bwhile of integer * qubit * com *)

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
  | p :: [] -> Printf.sprintf "%.5f" p
  | p :: l -> 
     let s = Printf.sprintf "%.5f" p in 
     let sl = unparse_par l in
     s ^ "," ^ sl

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
  | Skip -> 
     "Skip"
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

let test_p1 =
  let c1 = Init (Qvar "q0") in
  let c2 = (Uapp (Gate ("H", [0.5]), [Qvar "q0"])) in
  Seq(Seq(c1, c2), c2)

let test_p2 =
  let c0 = Init (Qvar "q0") in
  let c1 = Init (Qvar "q1") in
  let c2 = (Uapp (Gate ("H", [0.5]), [Qvar "q0"])) in
  let c3 = (Uapp (Gate ("CNOT", [0.3]), [Qvar "q0"; Qvar "q1"])) in
  Seq(Seq(c0, c1), Seq(c2, c3))

let () =
  let c = test_p2 in
  (* let c = Skip in  *)
  let s = unparse_com c in
  print_endline s




(* open Core.Std *)


type qid = string
type uid = string

(* uid "X"
uid "Y"
uid "Z"

uid "XX"
uid "YY"
uid "ZZ" *)

type parid = string
(* theta1, theta2 etc  *)
type par = parid list
(* The list of parameters  *)

type evaledparid = float
type evaledpar = evaledparid list
(* type evalpar = par -> evaledpar *)
(* evaluating a list of parameters, e.g: (theta_1,theta_2)\mapsto
(0,5,0.7) . Semantics. No need yet. *)



type qbit =
| Qvar of qid

type qlist = qbit list




(* below: syntax for parameterized progs (\S 4.1) *)
type unitary = 
| Gate of uid * par
| OneBRot of uid * par (* e^{-i * theta/2 * X}, etc *)
| TwoBRot of uid * par 
(* e^{-i * theta/2 * X\otimes X}, etc*)
(* The above: parameterized unitary, 
  e.g U(theta_1, theta_2)*)

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
  | OneBRot (g, t) -> 
     let st = unparse_par t in 
     g ^ "(" ^ st ^ ")"
  | TwoBRot (g, t) -> 
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
     "case M(" ^ q ^ ") = 0 then " ^ s1 ^ "else " ^ s2
  | Bwhile (num, qb, u1) ->
     let nt = Printf.sprintf "%d" num in
     let q = unparse_qbit qb in
     let s1 = unparse_com u1 in
     "while^" ^ nt ^ " M(" ^ q ^ ")= 1 do " ^ s1   
(* endof syntax for parameterized progs (\S 4.1) *)


(* below: syntax for UNparameterized progs (\S 3.1) *)

type unparunitary = 
| UnparGate of uid * evaledpar 
| UnparOneBRot of uid * evaledpar 
| UnparTwoBRot of uid * evaledpar 

(* The above:  e.g U(0.5, 0.7). *)

 type uNcom = 
| UNAbort of qlist
| UNSkip of qlist
| UNInit of qbit
| UNUapp of unparunitary * qlist
| UNSeq of uNcom * uNcom
| UNCase of qbit * uNcom * uNcom
| UNBwhile of int * qbit * uNcom 


let rec unparse_evaledpar theta : string =
   match theta with
   | [] -> ""
   | p :: [] -> Printf.sprintf "%5f" p
   | p :: l ->  
      let sl = unparse_evaledpar l in
      Printf.sprintf "%5f" p ^ "," ^ sl


let unparse_unparunitary u : string =
  match u with
  | UnparGate (g, t) -> 
     let st = unparse_evaledpar t in 
     g ^ "(" ^ st ^ ")"
  | UnparOneBRot (g, t) -> 
     let st = unparse_evaledpar t in 
     g ^ "(" ^ st ^ ")"
  | UnparTwoBRot (g, t) -> 
     let st = unparse_evaledpar t in 
     g ^ "(" ^ st ^ ")"

let rec unparse_UNcom c : string = 
  match c with
  | UNAbort ql -> 
     let sql = unparse_qlist ql in
     "Abort[" ^ sql ^ "]"
  | UNSkip ql -> 
     let sql = unparse_qlist ql in
      "Skip[" ^ sql ^ "]"
  | UNInit q -> 
     let s = unparse_qbit q in 
     s ^ ":=∣0⟩"
  | UNUapp (u, ql) -> 
     let su = unparse_unparunitary u in 
     let sql = unparse_qlist ql in 
     sql ^ ":=" ^ su ^ "[" ^ sql ^ "]"
  | UNSeq (c1, c2) ->
     let s1 = unparse_UNcom c1 in
     let s2 = unparse_UNcom c2 in
     s1 ^ "; " ^ s2
  | UNCase (qb, u1, u2) ->
     let q = unparse_qbit qb in
     let s1 = unparse_UNcom u1 in
     let s2 = unparse_UNcom u2 in
     "case M(" ^ q ^ ") = 0 then " ^ s1 ^ "else " ^ s2
  | UNBwhile (num, qb, u1) ->
     let nt = Printf.sprintf "%d" num in
     let q = unparse_qbit qb in
     let s1 = unparse_UNcom u1 in
     "while^" ^ nt ^ " M(" ^ q ^ ")= 1 do " ^ s1   
(* endof syntax for UNparameterized progs (\S 3.1) *)



(* below: syntax for non-deterministic parameterized progs (\S 5.1) *)
type underlineUnitary = 
| UnderlineGate of uid * par
| UnderlineOneBRot of uid * par
| UnderlineTwoBRot of uid * par

(* The above: parameterized unitary with non-det type (before compilation),
 e.g \underline{U(theta_1, theta_2)} *)

type underlineCom =
| UnderlineAbort of qlist
| UnderlineSkip of qlist
| UnderlineInit of qbit
| UnderlineUapp of underlineUnitary * qlist
| UnderlineSeq of underlineCom * underlineCom
| UnderlineCase of qbit * underlineCom * underlineCom
| UnderlineBwhile of int * qbit * underlineCom 
| UnderlineAdd of underlineCom * underlineCom



let unparse_UnderlineUnitary u : string =
  match u with
  | UnderlineGate (g, t) -> 
     let st = unparse_par t in 
     g ^ "(" ^ st ^ ")"
  | UnderlineOneBRot (g, t) -> 
     let st = unparse_par t in 
     g ^ "(" ^ st ^ ")"
  | UnderlineTwoBRot (g, t) -> 
     let st = unparse_par t in 
     g ^ "(" ^ st ^ ")"


let rec unparse_UnderlineCom c : string = 
  match c with
  | UnderlineAbort ql -> 
     let sql = unparse_qlist ql in
     "Abort[" ^ sql ^ "]"
  | UnderlineSkip ql -> 
     let sql = unparse_qlist ql in
      "Skip[" ^ sql ^ "]"
  | UnderlineInit q -> 
     let s = unparse_qbit q in 
     s ^ ":=∣0⟩"
  | UnderlineUapp (u, ql) -> 
     let su = unparse_UnderlineUnitary u in 
     let sql = unparse_qlist ql in 
     sql ^ ":=" ^ su ^ "[" ^ sql ^ "]"
  | UnderlineSeq (c1, c2) ->
     let s1 = unparse_UnderlineCom c1 in
     let s2 = unparse_UnderlineCom c2 in
     s1 ^ "; " ^ s2
  | UnderlineCase (qb, u1, u2) ->
     let q = unparse_qbit qb in
     let s1 = unparse_UnderlineCom u1 in
     let s2 = unparse_UnderlineCom u2 in
     "case M(" ^ q ^ ") = 0 then " ^ s1 ^ "else " ^ s2
  | UnderlineBwhile (num, qb, u1) ->
     let nt = Printf.sprintf "%d" num in
     let q = unparse_qbit qb in
     let s1 = unparse_UnderlineCom u1 in
     "while^" ^ nt ^ " M(" ^ q ^ ")= 1 do " ^ s1
  | UnderlineAdd (u1, u2) -> 
     let s1 = unparse_UnderlineCom u1 in
     let s2 = unparse_UnderlineCom u2 in
    s1 ^ "+" ^ s2

(* Note that when printing we will have different lengths
 of the " ___ " lines and length correspond to the number
 of recursive layers. *)    
(* endof syntax for parameterized progs (\S 5.1) *)



(* Test cases: test_p2 for parameterized (\S 4.1), 
test_p3 for unparameterized (\S 3.1),
test_p4 for parameterized non-det (\S 5.1) *)

(*  let test_p1 =
  let c1 = Init (Qvar "q0") in
  let c2 = (Uapp (Gate ("H", ["theta"]), [Qvar "q0"])) in
  Seq(Seq(c1, c2), c2)  *)

(***  p2 - p4: Small Test Cases for syntax sanity check  ***)
(*** 
  let test_p2 =
  let c0 = Init (Qvar "q0") in
  let c1 = Init (Qvar "q1") in
  let c2 = (Uapp (Gate ("H", ["theta_1"]), [Qvar "q0"])) in
  let c3 = (Uapp (Gate ("CNOT", ["theta_2"]), [Qvar "q0"; Qvar "q1"])) in
  let c4 = Seq(Seq(c0, c1), Seq(c2, c3)) in
  let guard = Qvar "q3" in 
  Bwhile (2, guard, c4)     
 
  


let test_p3 =
  let c0 = UNInit (Qvar "q0") in
  let c1 = UNInit (Qvar "q1") in
  let c2 = (UNUapp (UnparTwoBRot ("XX", [0.99]), [Qvar "q0"; Qvar "q1"])) in
  let c3 = (UNUapp (UnparOneBRot ("Y", [0.66]), [Qvar "q0"; Qvar "q1"])) in
  let c4 = UNSeq(UNSeq(c0, c3), UNSeq(c2, c1)) in
  let c5 = UNSeq(UNSeq(c2, c0), UNSeq(c1, c3)) in 
  let guard = Qvar "q3" in 
  UNCase (guard, c4, c5)


let test_p4 =
  let c0 = UnderlineInit (Qvar "q0") in
  let c1 = UnderlineInit (Qvar "q1") in
  let c2 = (UnderlineUapp (UnderlineOneBRot ("X", ["theta_1"]), [Qvar "q0"])) in
  let c3 = (UnderlineUapp (UnderlineTwoBRot ("YY", ["theta_2"]), [Qvar "q1"])) in
  let c4 = UnderlineSeq(c1, c2) in
  let c5 = UnderlineAdd(c0, c3) in 
  let guard = Qvar "q3" in 
  UnderlineCase (guard, c4, c5)

  let () =  
  let c2 = test_p2 in 
  let s2 = unparse_com c2 in
  print_endline s2

  let () =
  let cnextline = "\r" in 
  print_endline cnextline

  let () =  
  let c3 = test_p3 in 
  let s3 = unparse_UNcom c3 in
  print_endline s3

  let () =
  let cnextline = "\r" in 
  print_endline cnextline
  
  let () =  
  let c4 = test_p4 in 
  let s4 = unparse_UnderlineCom c4 in
  print_endline s4

  let () =
  let cnextline = "\r" in 
  print_endline cnextline

  ***)


(***** Code Transformation Rules (Figure 6) *****)


(* First, given any normal parameterized program, we view it
as the corresponding non-det parameterized program. A func 
tranforming it naturally (i.e. takes a normal and returns a non-det0) 
is in order.*)

let normalUnitToNonDetUnit u : underlineUnitary =
  match u with 
  | Gate (u, pl) -> UnderlineGate(u, pl)
  | OneBRot (u, pl) -> UnderlineOneBRot(u, pl)
  | TwoBRot (u, pl) -> UnderlineTwoBRot(u, pl)



let rec normalToNonDet u : underlineCom =
  match u with 
  | Abort ql -> UnderlineAbort ql
  | Skip ql -> UnderlineSkip ql 
  | Init q -> UnderlineInit q
  | Uapp (u, ql) -> UnderlineUapp (normalUnitToNonDetUnit (u), ql)
  | Seq (c1, c2) -> UnderlineSeq (normalToNonDet c1, normalToNonDet c2)
  | Case (qb, u1, u2) ->
     UnderlineCase(qb, normalToNonDet u1, normalToNonDet u2)
  | Bwhile (num, qb, u1) ->
     UnderlineBwhile(num, qb, normalToNonDet u1)


(*** Test case p5: checking the "inclusion rule" for converting det parameterized
progs to non-det. ***)

(***
let test_p5 =
  let c0 = Init (Qvar "q0") in
  let c1 = Init (Qvar "q1") in
  let c2 = (Uapp (OneBRot ("X", ["theta_1"]), [Qvar "q0"])) in
  let c3 = (Uapp (TwoBRot ("ZZ", ["theta_2"]), [Qvar "q0"; Qvar "q1"])) in
  let c4 = Seq(Seq(c0, c1), Seq(c2, c3)) in
  normalToNonDet (c4)


let () =  
  let c5 = test_p5 in 
  let s5 = unparse_UnderlineCom c5 in
  print_endline s5


let () =
  let cnextline = "\r" in 
  print_endline cnextline

***)

(* TODO: abstractly define controled unitary and its unparse;
then one can compose them to make the thing work!

That's symantics. We ignore that as of now *)
(* type cRot = 
|ControledRot of uid * par * qlist -> underlineCom 


let test_p7 =
ControledRot (X ["theta_1"] ["Ancilla"]) 

let () =  
  let c7 = test_p7 in 
  let s7 = unparse_UnderlineCom c7 in
  print_endline s7 *)

(* let rotationConvertToControlled (uuu: uid) (t: par) (ql: qlist) : underlineCom = *)

(* let codeTransformationUnitary uuu t qli: underlineCom = *)


(* We do syntactical transformation so we only need to explicitly write 
the control as some string without having to introduce the concept of
tensor (as well as the linear algebra underneath. Still needs some work. *)
(* Still TODO: define unitary differentiation rule first, then use 
that rule in codeTransformation. *)


(*To get the well-behaved BWhile code transformation rule one has to be
able to compute the qlist of a command. Helpers below: *)


let rec appendWithoutDuplicate l1 l2 : qlist = 
  match l2 with 
  | [] -> l1 
  | x :: l -> (match (List.mem x l1) with 
    | false -> appendWithoutDuplicate (List.append l1 [x]) l
    | true -> appendWithoutDuplicate l1 l
  )

let rec qListOfCom u : qlist =
  match u with 
  | UnderlineAbort ql -> 
     ql
  | UnderlineSkip ql -> 
     ql
  | UnderlineInit q -> 
     [q]
  | UnderlineUapp (_, ql) -> 
     ql
  | UnderlineSeq (c1, c2) ->
     appendWithoutDuplicate (qListOfCom c1) (qListOfCom c2) 
  | UnderlineCase (qb, u1, u2) ->
     appendWithoutDuplicate [qb] (appendWithoutDuplicate (qListOfCom u1) (qListOfCom u2)) 
  | UnderlineBwhile (_, qb, u1) ->
     appendWithoutDuplicate [qb] (qListOfCom u1)
  | UnderlineAdd (u1, u2) -> 
     appendWithoutDuplicate (qListOfCom u1) (qListOfCom u2) 

 let rec qListOfUnparCom u : qlist =
  match u with 
  | Abort ql -> 
     ql
  | Skip ql -> 
     ql
  | Init q -> 
     [q]
  | Uapp (_, ql) -> 
     ql
  | Seq (c1, c2) ->
     appendWithoutDuplicate (qListOfUnparCom c1) (qListOfUnparCom c2) 
  | Case (qb, u1, u2) ->
     appendWithoutDuplicate [qb] (appendWithoutDuplicate (qListOfUnparCom u1) (qListOfUnparCom u2)) 
  | Bwhile (_, qb, u1) ->
     appendWithoutDuplicate [qb] (qListOfUnparCom u1)
(*********
Code Compilation below, p19.
********)


let rec codeTransformation u parid: underlineCom = 
match u with
| UnderlineAbort ql -> UnderlineAbort (appendWithoutDuplicate ql [Qvar "A"])
| UnderlineSkip ql -> UnderlineAbort (appendWithoutDuplicate ql [Qvar "A"])
| UnderlineInit q -> UnderlineAbort (appendWithoutDuplicate [q] [Qvar "A"]) 
| UnderlineUapp (uu, ql) -> (match uu with
  | UnderlineOneBRot (uuu, t) -> ( match (List.mem parid t) with 
    | true ->  UnderlineSeq(UnderlineSeq(UnderlineUapp(UnderlineGate("H",[""]),[Qvar "A"]),
      UnderlineUapp(UnderlineGate("C-" ^ uuu,t), (appendWithoutDuplicate ql [Qvar "A"]))),
      UnderlineUapp(UnderlineGate("H",[""]),[Qvar "A"])) 
    | false -> UnderlineAbort (appendWithoutDuplicate ql [Qvar "A"]) )
  | UnderlineTwoBRot (uuu, t) -> ( match (List.mem parid t) with 
    | true -> UnderlineSeq(UnderlineSeq(UnderlineUapp(UnderlineGate("H",[""]),[Qvar "A"]),
      UnderlineUapp(UnderlineGate("C-" ^ uuu,t), (appendWithoutDuplicate ql [Qvar "A"]))),
      UnderlineUapp(UnderlineGate("H",[""]),[Qvar "A"]))
    (* nderlineUapp(UnderlineGate(uuu,t), (List.append ql [Qvar "Place holder 20, change later."]))*)
    | false -> UnderlineAbort (appendWithoutDuplicate ql [Qvar "A"]) )(* UnderlineAbort (List.append ql [Qvar "Place holder 21, change later."]) *)
  (*_ -> UnderlineAbort (List.append ql [Qvar "Please only pass me Rotations."]) *) 
  (* ( match (List.mem parid t) with 
    | true -> UnderlineAbort (List.append ql [Qvar "Please only pass me Rotations."]) 
    (*UnderlineUapp(UnderlineGate(uuu,t), (List.append ql [Qvar "Place holder 00, change later."]))*)
    | false -> UnderlineAbort (List.append ql [Qvar "Please only pass me Rotations."]) ) *)
  |_ -> UnderlineAbort (appendWithoutDuplicate ql [Qvar "Please only pass me Rotations."]) 
  )
 | UnderlineAdd (u1, u2) -> UnderlineAdd (codeTransformation u1 parid,
  codeTransformation u2 parid)
 | UnderlineSeq (u1, u2) -> UnderlineAdd (UnderlineSeq (codeTransformation u1 parid,
 u2), UnderlineSeq (u1, codeTransformation u2 parid) )
 | UnderlineCase (qb, u1, u2) -> UnderlineCase (qb, codeTransformation u1 parid, 
 codeTransformation u2 parid)
 | UnderlineBwhile (num, qb, u1) -> (match (num > 1) with 
    | false -> (match (num = 0) with 
      | true -> UnderlineAbort ([Qvar "Error! Need T > 0."])
      | false -> codeTransformation
        (UnderlineCase (qb, UnderlineSkip (appendWithoutDuplicate (qListOfCom u1) [Qvar "A"]),
        UnderlineSeq (u1,
          UnderlineAbort (appendWithoutDuplicate (qListOfCom u1) [Qvar "A"]))
        )) parid
      )   
    (* Next, T >= 2*)
    | true -> codeTransformation 
        (UnderlineCase (qb,UnderlineSkip (appendWithoutDuplicate (qListOfCom u1) [Qvar "A"]),
        UnderlineSeq (u1, 
          UnderlineBwhile (num-1, qb, u1) )
        )) parid
    ) 
 (* | _ -> UnderlineAbort ([Qvar "Place holder, fill later."]) *)
(* Place holder message: please Express Unitary as 
    Product of 1qb or 2qb rotations, as that's the only
    things that our rules allow. *)
(* Then it's application of code tranformation rules. the rules
should still be non-det to non-det, as the paper said.*)




  (* TODO: We need to be assign values to parameters,
     so need a function type assign: parid -> float: done It's really
     semantics so we commented it out.

     We also need to define a function type mapping from parameterized
     unitary to actual unitaries, i.e.
     a function type that takes a (parameterized)unitary * assign, returns
     unparameterized unitary. This function assigns
     real values to the parameters in the parameterized unitary,
     and outputs an unparameterized unitary. The assignment is done
     in compliance with the "assign" function *)



(* For the Compilation rules let's just use list to represent multiset, since
we don't care about order and we throw unnecessary aborts away on the go.*)

(*** Compilation rules: Fig 5, p14 ***)



(* A helper. e.g. returnConcat {|U1, U2|} {|U3, U3|} = {|U1U3, U1U3, 
U2U3, U2U3|} *)


let rec returnConcat l1 l2 : com list =
  match l1 with 
  | [] -> []
  | x :: l -> (match l2 with
    | [] -> []
    | x' :: l' -> List.append 
    (List.append [Seq(x, x')] (returnConcat [x] l'))
    (List.append (returnConcat l [x']) (returnConcat l l'))
  )



(*** let addAbort times l  : list com = 
  let l0 =  [] in
  let counter = 1 in 
  let () = in 
  while counter <= times do l0 = List.append l0 [Abort (qListOfUnparCom (List.hd l2)) ]; 
  counter = counter + 1 done  ***)
  
(* Another helper: Takes C1 C2 two lists of regular commands, returns
  a filled list. If no need to fill, return C2. 
*)

let fillUp l1 l2 : com list =
  let diff = (List.length l1) - (List.length l2) in 
  (match (diff > 0) with
  | true -> let produceAbort (n : int) : com = 
            (match n with 
            | _ -> Abort (qListOfUnparCom (List.hd l2))
            ) in 
            let op = List.init diff produceAbort in 
            List. append l2 op
  | false -> (match (diff = 0) with 
    |false  -> let produceAbort (n : int) : com = 
            (match n with 
            | _ -> Abort (qListOfUnparCom (List.hd l1))
            ) in 
              let op = List.init ((List.length l2) - (List.length l1)) produceAbort in 
            List. append l1 op
    |true -> l2 
    )
  )

(* take two lists of same length, return a set of cases with same length *)
let rec createIf qb l1 l2 : com list = 
  match l1 with 
  | [] -> (match l2 with 
    | [] -> []
    | _ :: _ -> [Abort ([Qvar "Lists of diff length!"])]
  )
  | x::l -> ( match l2 with 
    | [] -> [Abort ([Qvar "Lists of diff length!"])]
    | x' :: l' -> List.append [Case (qb, x, x')] (createIf qb l l')
  )


  (* List.append [Case (qb, l1.hd, l2.hd)] (recCreateIf qb l1.) *)

let rec codeCompilation u : com list =
  match  u with
  | UnderlineAbort ql -> [Abort ql]
  | UnderlineSkip ql -> [Skip ql]
  | UnderlineInit q -> [Init q]
  | UnderlineUapp (u, ql) -> (match u with
    | UnderlineGate (uu, par) -> [Uapp (Gate (uu, par), ql)]
    | UnderlineOneBRot (uu, par) -> [Uapp (OneBRot (uu, par),ql)]
    | UnderlineTwoBRot (uu, par) -> [Uapp (TwoBRot (uu, par),ql)]
  )
  | UnderlineSeq (u1, u2) -> ( match (codeCompilation u1 = [Abort (qListOfCom u1)])
  with 
    | false -> (match (codeCompilation u2 = [Abort (qListOfCom u2)])
  with 
      |false -> returnConcat (codeCompilation u1) (codeCompilation u2)
      |true -> [Abort (qListOfCom u)]
    )
    | true -> [Abort (qListOfCom u)]
  )
  | UnderlineAdd (u1, u2) -> ( match (codeCompilation u1 = [Abort (qListOfCom u1)])
  with 
    | false -> (match (codeCompilation u2 = [Abort (qListOfCom u2)]) with 
      |false -> List.append (codeCompilation u1) (codeCompilation u2)
      |true -> codeCompilation u1
    )
    | true -> (match (codeCompilation u2 = [Abort (qListOfCom u2)])
  with 
      |false -> codeCompilation u2
      |true -> [Abort (qListOfCom u)]
    )
  )
 
 | UnderlineCase (qb, u1, u2) -> let l1 = codeCompilation u1 in 
                                 let l2 = codeCompilation u2 in 
                                 let diff = List.length l1 - List.length l2 in 
                                 (match (diff > 0) with 
                                  | true -> let filled2 = fillUp l1 l2 in 
                                            createIf qb l1 filled2
                                  | false -> (match (diff = 0) with
                                    |false -> let filled1 = fillUp l1 l2 in 
                                            createIf qb filled1 l2
                                    |true -> createIf qb l1 l2
                                  )

 )
 (* UnderlineCase (qb, codeTransformation u1 parid, 
 codeTransformation u2 parid) *)
 | UnderlineBwhile (num, qb, u1) -> (match (num > 1) with 
    | false -> (match (num = 0) with 
      | true -> [Abort ([Qvar "Error! Need T > 0."])]
      | false -> codeCompilation
        (UnderlineCase (qb, UnderlineSkip (qListOfCom u1),
        UnderlineSeq (u1,
          UnderlineAbort (qListOfCom u1) )
        )) 
      )   
    (* Next, T >= 2*)
    | true -> codeCompilation
        (UnderlineCase (qb,UnderlineSkip (qListOfCom u1) ,
        UnderlineSeq (u1, 
          UnderlineBwhile (num-1, qb, u1) )
        )) 
    ) 


let test_p7 = 
  let c0 = Init (Qvar "q0") in
  let c1 = Init (Qvar "q1") in
  let c2 = (Uapp (OneBRot ("X", ["theta_1"]), [Qvar "q0"])) in 
  let c3 = (Uapp (TwoBRot ("ZZ", ["theta_2"]), [Qvar "q0"; Qvar "q1"])) in
  let c4 = Seq(Seq(c0, c1), Seq(c2, c3)) in 
  let ndprog = normalToNonDet (c4) in 
  let ndprog2 = UnderlineBwhile (3, Qvar "qm", ndprog) in 
  (* let ndprog = normalToNonDet (c3) in *)
  let papar = "theta_1" in 
  let daoChengXu = codeTransformation ndprog2 papar in 
  (* let opDaoChengXU = *) codeCompilation daoChengXu

 let rec printList li =
 match li with 
 | [] -> let cnextline = "\r End of printing \r" in 
                 print_endline cnextline
 | x :: l' -> let cnextline = "\r" in 
             let s6 = unparse_com x in
             print_endline s6 ; print_endline cnextline; printList l'
 let () =  printList test_p7 (* match (List.length opDaoChengXU > 0) with
  | false -> let cnextline = "WTF, empty set of progs?\r" in 
                 print_endline cnextline
  let c6 = test_p7 in 
  let s6 = unparse_UnderlineCom c6 in
  print_endline s6 *)

let () =
  let cnextline = "\r" in 
  print_endline cnextline


  (*Garbage code: match (List.mem parid t2) with
    | true -> UnderlineAbort (List.append ql [Qvar "Place holder, change later."])
    | false -> UnderlineAbort (List.append ql [Qvar "Place holder, change later."])*)

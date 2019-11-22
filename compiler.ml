

type qid = string
type uid = string



type parid = string
(* theta1, theta2 etc  *)
type par = parid list
(* The list of parameters  *)

 type evaledparid = float
type evaledpar = evaledparid list 

(* type evalpar = par -> evaledpar *)
(* evaluating a list of parameters, e.g: (theta_1,theta_2)\mapsto
(0,5,0.7) . Semantics. No need yet for symbolic derivatives. *)



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

let rec indent n : string =
  match n with
  | 0 -> ""
  | x -> "    " ^ indent (x-1)

let rec unparse_com c lv : string = 
  let it = indent lv in 
  match c with
  | Abort ql -> 
     let sql = unparse_qlist ql in
     it ^ "abort[" ^ sql ^ "]"
  | Skip ql -> 
     let sql = unparse_qlist ql in
      it ^ "skip[" ^ sql ^ "]"
  | Init q -> 
     let s = unparse_qbit q in 
     it ^ s ^ ":=∣0⟩"
  | Uapp (u, ql) -> 
     let su = unparse_unitary u in 
     let sql = unparse_qlist ql in 
     it ^ sql ^ ":=" ^ su ^ "[" ^ sql ^ "]"
  | Seq (c1, c2) ->
     let s1 = unparse_com c1 lv in
     let s2 = unparse_com c2 lv in
     s1 ^ "; \n" ^ s2
  | Case (qb, u1, u2) ->
     let q = unparse_qbit qb in
     let s1 = unparse_com u1 (lv+1) in
     let s2 = unparse_com u2 (lv+1) in
     it ^ "case M(" ^ q ^ ") = 0 then \n" ^ s1 ^ "\n" ^ it ^ "else\n" ^ s2 ^ "\n" ^ it ^ "end"
  | Bwhile (num, qb, u1) ->
     let nt = Printf.sprintf "%d" num in
     let q = unparse_qbit qb in
     let s1 = unparse_com u1 (lv+1) in
     it ^ "while^" ^ nt ^ " M(" ^ q ^ ")= 1 do \n" ^ s1 ^ "\n" ^ it ^ "od"  
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
     s1 ^ "; \n" ^ s2
  | UNCase (qb, u1, u2) ->
     let q = unparse_qbit qb in
     let s1 = unparse_UNcom u1 in
     let s2 = unparse_UNcom u2 in
     "case M(" ^ q ^ ") = 0 then \n" ^ s1 ^ "\nelse\n" ^ s2 ^ "\nend"
  | UNBwhile (num, qb, u1) ->
     let nt = Printf.sprintf "%d" num in
     let q = unparse_qbit qb in
     let s1 = unparse_UNcom u1 in
     "while^" ^ nt ^ " M(" ^ q ^ ")= 1 do \n" ^ s1 ^ "\n od"
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


let rec unparse_UnderlineCom c lv : string = 
  let it = indent lv in 
  match c with
  | UnderlineAbort ql -> 
     let sql = unparse_qlist ql in
     it ^ "Abort[" ^ sql ^ "]"
  | UnderlineSkip ql -> 
     let sql = unparse_qlist ql in
      it ^ "Skip[" ^ sql ^ "]"
  | UnderlineInit q -> 
     let s = unparse_qbit q in 
     it ^ s ^ ":=∣0⟩"
  | UnderlineUapp (u, ql) -> 
     let su = unparse_UnderlineUnitary u in 
     let sql = unparse_qlist ql in 
     it ^ sql ^ ":=" ^ su ^ "[" ^ sql ^ "]"
  | UnderlineSeq (c1, c2) ->
     let s1 = unparse_UnderlineCom c1 lv in
     let s2 = unparse_UnderlineCom c2 lv in
     s1 ^ "; \n" ^ s2
  | UnderlineCase (qb, u1, u2) ->
     let q = unparse_qbit qb in
     let s1 = unparse_UnderlineCom u1 (lv+1) in
     let s2 = unparse_UnderlineCom u2 (lv+1) in
     it ^ "case M(" ^ q ^ ") = 0 then \n" ^ s1 ^ it ^ "\nelse\n" ^ s2 ^ it ^ "\nend"
  | UnderlineBwhile (num, qb, u1) ->
     let nt = Printf.sprintf "%d" num in
     let q = unparse_qbit qb in
     let s1 = unparse_UnderlineCom u1 (lv+1) in
     it ^ "while^" ^ nt ^ " M(" ^ q ^ ")= 1 do \n" ^ s1 ^ it ^ "\n od"
  | UnderlineAdd (u1, u2) -> 
     let s1 = unparse_UnderlineCom u1 lv in
     let s2 = unparse_UnderlineCom u2 lv in
    s1 ^ "\n" ^ it ^ "+" ^ "\n" ^ s2

(* Note that when printing we will have different lengths
 of the " ___ " lines and length correspond to the number
 of recursive layers. *)    
(* endof syntax for parameterized progs (\S 5.1) *)







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








(*To get the well-behaved BWhile code transformation rule one has to be
able to compute the qlist of a command. Helpers below: *)


let rec appendWithoutDuplicate l1 l2 : qlist = 
  (* let rl1 = List.rev l1 in *)
  match l1 with 
  | [] -> l2
  | x :: l -> (match (List.mem x l2) with 
    | false -> appendWithoutDuplicate l (x:: l2)
    | true -> appendWithoutDuplicate l l2
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
     appendWithoutDuplicate (List.rev(qListOfCom c1)) (qListOfCom c2) 
  | UnderlineCase (qb, u1, u2) ->
     appendWithoutDuplicate [qb] (appendWithoutDuplicate (List.rev(qListOfCom u1)) (qListOfCom u2)) 
  | UnderlineBwhile (_, qb, u1) ->
     appendWithoutDuplicate [qb] (qListOfCom u1)
  | UnderlineAdd (u1, u2) -> 
     appendWithoutDuplicate (List.rev(qListOfCom u1)) (qListOfCom u2) 

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
     appendWithoutDuplicate (List.rev(qListOfUnparCom c1)) (qListOfUnparCom c2) 
  | Case (qb, u1, u2) ->
     appendWithoutDuplicate [qb] (appendWithoutDuplicate (List.rev(qListOfUnparCom u1)) (qListOfUnparCom u2)) 
  | Bwhile (_, qb, u1) ->
     appendWithoutDuplicate [qb] (qListOfUnparCom u1)
(*********
Code Compilation below.
********)


let rec codeTransformation u parid: underlineCom = 
match u with
| UnderlineAbort ql -> UnderlineAbort (appendWithoutDuplicate [Qvar "A"] ql)
| UnderlineSkip ql -> UnderlineAbort (appendWithoutDuplicate [Qvar "A"] ql)
| UnderlineInit q -> UnderlineAbort (appendWithoutDuplicate [Qvar "A"] [q]) 
| UnderlineUapp (uu, ql) -> (match uu with
  | UnderlineOneBRot (uuu, t) -> ( match (List.mem parid t) with 
    | true ->  UnderlineSeq(UnderlineSeq(UnderlineUapp(UnderlineGate("H",[""]),[Qvar "A"]),
      UnderlineUapp(UnderlineGate("C-" ^ uuu,t), (appendWithoutDuplicate [Qvar "A"] ql))),
      UnderlineUapp(UnderlineGate("H",[""]),[Qvar "A"])) 
    | false -> UnderlineAbort (appendWithoutDuplicate [Qvar "A"] ql) )
  | UnderlineTwoBRot (uuu, t) -> ( match (List.mem parid t) with 
    | true -> UnderlineSeq(UnderlineSeq(UnderlineUapp(UnderlineGate("H",[""]),[Qvar "A"]),
      UnderlineUapp(UnderlineGate("C-" ^ uuu,t), (appendWithoutDuplicate [Qvar "A"] ql))),
      UnderlineUapp(UnderlineGate("H",[""]),[Qvar "A"]))
    | false -> UnderlineAbort (appendWithoutDuplicate [Qvar "A"] ql) )
  |_ -> UnderlineAbort (appendWithoutDuplicate [Qvar "A"] ql)
  (* Note: H, CNOT derivative is Abort; assume we don't do higher order derivative so
  CX~CZ, CXX~CZZ are never differentiated. *) 
  )
 | UnderlineAdd (u1, u2) -> UnderlineAdd (codeTransformation u1 parid,
  codeTransformation u2 parid)
 | UnderlineSeq (u1, u2) -> UnderlineAdd (UnderlineSeq (codeTransformation u1 parid,
 u2), UnderlineSeq (u1, codeTransformation u2 parid) )
 | UnderlineCase (qb, u1, u2) -> UnderlineCase (qb, codeTransformation u1 parid, 
 codeTransformation u2 parid)
 | UnderlineBwhile (num, qb, u1) -> (let bigli = appendWithoutDuplicate  [Qvar "A"] (qListOfCom u1) in 

    (match (num > 1) with 
    | false -> (match (num = 0) with 
      | true -> UnderlineAbort ([Qvar "Error! Need T > 0."])
      | false -> ( 
        codeTransformation
        (UnderlineCase (qb, UnderlineSkip (bigli),
        UnderlineSeq (u1,
          UnderlineAbort (bigli) )
        )) parid)
      )   
    (* Next, T >= 2*)
    | true -> codeTransformation 
        (UnderlineCase (qb,UnderlineSkip (bigli),
        UnderlineSeq (u1, 
          UnderlineBwhile (num-1, qb, u1) )
        )) parid
    )

  ) 
 



  



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
  | UnderlineSeq (u1, u2) -> ( let clcu = qListOfCom u in 
                               let cu1 = codeCompilation u1 in
                               let cu2 = codeCompilation u2 in 
      match (List.length cu1 =1 ) with 
      | false -> (
                  match (List.length cu2 = 1) with 
                  |false -> returnConcat cu1 cu2
                  |true -> (let  singleu2 = List.hd cu2 in 
                           match singleu2 with 
                           |Abort _ -> [Abort (clcu)]
                           |_ -> returnConcat cu1 [singleu2]

                         )
                )
      | true ->(  let singleu1 = List.hd cu1 in 
                  match singleu1 with 
                  | Abort _ -> [Abort (clcu)]
                  | _ -> (
                          match (List.length cu2 = 1) with 
                          |false -> returnConcat cu1 cu2
                          |true -> (let  singleu2 = List.hd cu2 in 
                           match singleu2 with 
                           |Abort _ -> [Abort (clcu)]
                           |_ -> returnConcat [singleu1] [singleu2]

                         )
                    )
                )

    
  


  )
  | UnderlineAdd (u1, u2) -> ( let clcu = qListOfCom u in 
                               let cu1 = codeCompilation u1 in 
                               match (List.length cu1 = 1) with 
                               |false -> (let cu2 = codeCompilation u2 in 
                                          match (List.length cu2 = 1) with
                                          |false -> List.append cu1 cu2 
                                          |true -> (let singleu2 = List.hd cu2 in 
                                                    match singleu2 with
                                                    |Abort _ -> cu1 
                                                    |_ -> List.append cu1 cu2 
                                                  ) 


                               )
                               |true -> (let singleu1 = List.hd cu1 in 
                                         let cu2 = codeCompilation u2 in 
                                                    match singleu1 with
                                                    |Abort _ -> (match (List.length cu2 = 1) with
                                                      | false -> cu2 
                                                      | true -> (let singleu2 = List.hd cu2 in
                                                                match singleu2 with 
                                                                |Abort _ -> [Abort (clcu)]
                                                                |_ -> [singleu2]

                                                    )
                                                    )

                                                    |_ -> (
                                                      match (List.length cu2 = 1) with
                                                      | false -> List.append cu1 cu2 
                                                      | true -> (let singleu2 = List.hd cu2 in
                                                                match singleu2 with 
                                                                |Abort _ -> [singleu1]
                                                                |_ -> List.append cu1 cu2 


                                                      ) 


                                                    )  
                                        ) 
     
    
  )

 
 | UnderlineCase (qb, u1, u2) -> let clcu = qListOfCom u in
                                 let l1 = codeCompilation u1 in 
                                 let l2 = codeCompilation u2 in 
                                 let ll1 = List.length l1 in 
                                 let ll2 = List.length l2 in
                                 let diff = ll1 - ll2 in 
                                 (match (diff > 0) with 
                                  | true -> let filled2 = fillUp l1 l2 in 
                                            createIf qb l1 filled2
                                  
                                  | false -> (match (diff = 0) with
                                    | false -> let filled1 = fillUp l1 l2 in 
                                            createIf qb filled1 l2
                                    | true -> (match (ll1 = 1) with 
                                      | false -> createIf qb l1 l2 
                                      | true -> (let singleu1 = List.hd l1 in 
                                                 let singleu2 = List.hd l2 in 
                                                 (match singleu1 with 
                                                 | Abort _ -> ( match singleu2 with
                                                    |Abort _ -> [Abort (clcu)]
                                                    |_ -> createIf qb [singleu1] [singleu2] 
                                                 )
                                                 | _ -> createIf qb l1 l2 

                                               )


                                      )

                                      )
                                    
                                  )

 )
 (* UnderlineCase (qb, codeTransformation u1 parid, 
 codeTransformation u2 parid) *)
 | UnderlineBwhile (num, qb, u1) -> (let clcu1 = qListOfCom u1 in
      (match (num > 1) with 
    | false -> (match (num = 0) with 
      | true -> [Abort ([Qvar "Error! Need T > 0."])]
      | false -> codeCompilation
        (UnderlineCase (qb, UnderlineSkip (clcu1)  ,
        UnderlineSeq (u1,
          UnderlineAbort (clcu1)  )
        )) 
      )   
    (* Next, T >= 2*)
    | true -> codeCompilation
        (UnderlineCase (qb,UnderlineSkip (clcu1) ,
        UnderlineSeq (u1, 
          UnderlineBwhile (num-1, qb, u1) )
        )) 
    ) 
    )



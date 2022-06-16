module Intro2

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim(ope, e1, e2) ->
        let resEval1 = eval e1 env
        let resEval2 = eval e2 env
        match ope with
        | "+" -> resEval1 + resEval2
        | "-" -> resEval1 * resEval2
        | "*" -> resEval1 - resEval2
        | "Max" -> max resEval1 resEval2 
        | "Min" -> min resEval1 resEval2 
        | "==" -> if resEval1 = resEval2 then 1 else 0 
        | _ -> failwith "unknown primitive"
    | If (e1, e2, e3) -> if (eval e1 env) <> 0 then (eval e2 env) else (eval e3 env) 

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

let testMin1  = 1 = (eval (Prim("Min", CstI 1, CstI 9)) []);;
let testMin2  = 9 = (eval (Prim("Min", CstI 9, CstI 9)) []);;
let testMin3  = 9 = (eval (Prim("Min", CstI 14, CstI 9)) []);;
let testMax1  = 9 = (eval (Prim("Max", CstI 1, CstI 9)) []);;
let testMax2  = 9 = (eval (Prim("Max", CstI 9, CstI 9)) []);;
let testMax3  = 14 = (eval (Prim("Max", CstI 14, CstI 9)) []);;
let testEQ1  = 0 = (eval (Prim("==", CstI 14, CstI 9)) []);;
let testEQ2  = 1 = (eval (Prim("==", CstI 9, CstI 9)) []);;
let allTest = testMin1 && testMin2 && testMin3 && testMax1 && testMax2 && testMax1 && testEQ1 && testEQ2


//1.2

type aexpr = 
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr

let aexpr1 =(Var "v", (Var "w", Var "z") |> Add) |> Sub
let aexpr2 =(CstI 2, aexpr1) |> Mul
let aexpr3 =(Var "x", (Var "y", (Var "z", Var "v") |> Add) |> Add) |> Add

let rec fmt ae =
  match ae with
  | CstI i          -> i.ToString ()
  | Var v           -> v
  | Add (ae1, ae2)  -> "("+(fmt ae1) + " + " + (fmt ae2)+")"
  | Mul (ae1, ae2)  -> "("+(fmt ae1) + " * " + (fmt ae2)+")"
  | Sub (ae1, ae2)  -> "("+(fmt ae1) + " - " + (fmt ae2)+")"

let fmtTest1 = (Sub(Var "x", CstI 34)) |> fmt = "(x - 34)"

let isValue ae v = match ae with | CstI i -> i=v | _ -> false

let rec simplify ae = 
  match ae with 
  | CstI i          -> CstI i
  | Var v           -> Var v
  | Add (ae1, ae2)  -> if isValue ae1 0 then simplify ae2 else if isValue ae2 0 then simplify ae1 else Add (simplify ae1, simplify ae2)
  | Sub (ae1, ae2)  -> if isValue ae2 0 then simplify ae1 else if ae1=ae2       then CstI 0       else Sub (simplify ae1, simplify ae2)
  | Mul (ae1, ae2)  ->
    if isValue ae1 1 then simplify ae2 else if isValue ae2 1 then simplify ae1
    else if isValue ae1 0 then CstI 0 else if isValue ae2 0 then CstI 0 else Mul (simplify ae1, simplify ae2)

let rec symbolicDifferentiation env ae =
  let sumDiff ae = symbolicDifferentiation env ae
  match ae with
  | CstI i          -> 0
  | Var "x"         -> 1
  | Var _           -> 0
  | Add (ae1, ae2)  -> (sumDiff ae1) + (sumDiff ae2)
  | Mul (ae1, ae2)  -> (sumDiff ae1) * (sumDiff ae2)
  | Sub (ae1, ae2)  -> (sumDiff ae1) - (sumDiff ae2)










module Intcomp1

//2.1
type expr = 
  | CstI of int
  | Var of string
  | Let of (string * expr) list * expr
  | Prim of string * expr * expr;;


let e1 = Let([("z", CstI 17)], Prim("+", Var "z", Var "z"));;
let e2 = Let([("z", CstI 17); ("z", CstI 22)], 
             Prim ("+", Prim("*", CstI 100, Var "z"), Var "z"));;
let e3 = Let([("z", Prim("-", CstI 5, CstI 4))], 
             Prim("*", CstI 100, Var "z"));;
let e4 = Prim("+", Prim("+", CstI 20, Let([("z", CstI 17)], 
                                          Prim("+", Var "z", CstI 2))),
                   CstI 30);;
let e5 = Prim("*", CstI 2, Let ([("x", CstI 3)], Prim("+", Var "x", CstI 4)));;


let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Let(lst, ebody) -> 
      lst
      |> List.fold (fun acc (name, assignment) ->(name, eval assignment acc) :: acc) env
      |> eval ebody
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim _            -> failwith "unknown primitive";;

let run e = eval e [];;

let e6 =Let([("z", CstI 17); ("y", CstI 22)], 
             Prim ("+", Prim("*", Var "y", Var "y"), Var "z"));; 
             
let eExample1 =Let([("x1", Prim("+", CstI 5, CstI 7)); ("x2", Prim("*", Var "x1", CstI 2))], 
             Prim ("+", Var "x1", Var "x2"));; 

let evalTest1 = []|> eval e1 = 17+17          
let evalTest2 = []|> eval e6 = 22*22 + 17
let evalTestExample = [] |> eval eExample1 = 36


//2.2
let rec mem x vs = 
    match vs with
    | []      -> false
    | v :: vr -> x=v || mem x vr;;

let rec union (xs, ys) = 
    match xs with 
    | []    -> ys
    | x::xr -> if mem x ys then union(xr, ys)
               else x :: union(xr, ys);;

let rec minus (xs, ys) = 
    match xs with 
    | []    -> []
    | x::xr -> if mem x ys then minus(xr, ys)
               else x :: minus (xr, ys);;

let rec freevars e : string list =
    match e with
    | CstI i -> []
    | Var x  -> [x]
    | Let(lst, ebody) -> 
        let (free, bound) = 
            lst
            |> List.fold (fun (freeAcc, boundAcc) (name, assign) -> 
                union(freeAcc, minus(freevars assign, boundAcc)), union (boundAcc, [name])
                ) ([],[])
        union (free, minus(freevars ebody, bound))
    | Prim(ope, e1, e2) -> union (freevars e1, freevars e2);;

let closed2 e = (freevars e = []);;
let closed2Test = Let ([("x", CstI 7 )], Prim("+", Var "x", CstI 8)) |> closed2 
let closed2ExampleTest = Let ([("x1", Prim ("+", Var "x1" , CstI 7) )], Prim("+", Var "x1", CstI 8)) |> closed2 = false


//2.3

type texpr =                            (* target expressions *)
  | TCstI of int
  | TVar of int                         (* index into runtime environment *)
  | TLet of texpr * texpr               (* erhs and ebody                 *)
  | TPrim of string * texpr * texpr;;

let rec getindex vs x = 
    match vs with 
    | []    -> failwith "Variable not found"
    | y::yr -> if x=y then 0 else 1 + getindex yr x;;

let rec tcomp (e : expr) (cenv : string list) : texpr =
    match e with
    | CstI i -> TCstI i
    | Var x  -> TVar (getindex cenv x)
    | Let(lst, ebody) -> 
      match lst with 
      | [] -> tcomp ebody cenv 
      | (name, assing)::r -> 
          (tcomp assing cenv, tcomp (Let(r, ebody)) (name::cenv))
          |> TLet  
    | Prim(ope, e1, e2) -> TPrim(ope, tcomp e1 cenv, tcomp e2 cenv);;




//2.6

let rec eval26 e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Let(lst, ebody) -> 

      let rec evalLet newEnv lst=
        match lst with
        | [] -> newEnv
        | (name, assignment)::rest -> evalLet ((name, eval assignment env) :: newEnv) rest
      
      lst
      |>evalLet env
      |> eval26 ebody
    | Prim("+", e1, e2) -> eval26 e1 env + eval26 e2 env
    | Prim("*", e1, e2) -> eval26 e1 env * eval26 e2 env
    | Prim("-", e1, e2) -> eval26 e1 env - eval26 e2 env
    | Prim _            -> failwith "unknown primitive";;


let e7 =Let([("x", CstI 11)],
              Let([("x", CstI 22); ("y", Prim("+", Var "x", CstI 1))],
                Prim ("+", Var "x", Var "y"))
);; 

let eval26Test3 = eval26 e7 [] = 12+22
(* File Fun/ParseAndRun.fs *)

module ParseAndRun
open Absyn;;

let fromString = Parse.fromString;;

let eval = Fun.eval;;

let run e = eval e [];;


let test1 =
    Letfun("f1", ["x"], Prim("+", Var "x", CstI 1), 
        Call(Var "f1", [CstI 12]))
    |> run |> (=) 13;;

let test2 =
    Letfun("f1", ["x"; "y"; "x"], Prim("+", Var "x", Var "y"), 
        Call(Var "f1", [CstI 12; CstI 12; CstI 10]))
    |> run |> (=) 22;;
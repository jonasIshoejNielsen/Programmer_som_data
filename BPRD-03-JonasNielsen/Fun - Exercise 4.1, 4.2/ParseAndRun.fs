(* File Fun/ParseAndRun.fs *)

module ParseAndRun
open Absyn;;

let fromString = Parse.fromString;;

let eval = Fun.eval;;

let run e = eval e [];;



let A42 startVal= 
    Letfun (
        "sum",
        "i",
        If ((Prim("<",CstI 0, Var "i"), Prim("+", Var "i", Call(Var "sum", Prim("-", Var "i", CstI 1))), CstI 0)),
        Call(Var "sum", CstI startVal)
    )
    
let A42RunSpecial n = n |> A42 |> run
let A42Run = A42RunSpecial 1000


let B42 startVal= 
    Letfun (
        "3Power",
        "i",
        If (
            Prim("<",CstI 1, Var "i"),
            Prim("*", CstI 3, Call(Var "3Power", Prim("-", Var "i", CstI 1))),
            If (Prim("=",CstI 1, Var "i"), CstI 3, CstI 1)
           ),
        Call(Var "3Power", CstI startVal)
    )
let B42RunSpecial n = n |> B42 |> run
let B42Run = B42RunSpecial 8

let C42 (x, power)=
    Letfun (
        "xPower",
        "i",
        If (
            Prim("<",CstI 1, Var "i"),
            Prim("*", CstI x, Call(Var "xPower", Prim("-", Var "i", CstI 1))),
            If (Prim("=",CstI 1, Var "i"), CstI x, CstI 1)
           ),
        Letfun (
          "sumPow",
          "i",
          If (
              (Prim("<",CstI 0, Var "i"),
              Prim("+", Call(Var "xPower", Var "i"), Call(Var "sumPow", Prim("-", Var "i", CstI 1))),
              Call(Var "xPower", CstI 0))
          ),
          Call(Var "sumPow", CstI power)
        )
    )

let C42RunSpecial (x, pow) = (x, pow) |> C42 |> run
let C42Run = C42RunSpecial (3,11)


let D42 (startVal, power) =
    Letfun (
      "sumPow",
      "i",
      If (
        Prim("<",CstI 0, Var "i"),
            Prim("+",
              Call(Var "sumPow", Prim("-", Var "i", CstI 1)),
              Let ("nr", Var "i", 
                Letfun (
                    "NrPower",
                    "i",
                    If ( Prim("<", CstI 1, Var "i"), Prim("*", Var "nr", Call(Var "NrPower", Prim("-", Var "i", CstI 1))), If (Prim("=",CstI 1, Var "i"), Var "nr", CstI 1) ),
                    Call(Var "NrPower", CstI power) )
              )
            ),
            CstI 0
        ),
      Call(Var "sumPow", CstI startVal)
    )
let D42RunSpecial (maxBottom, pow) = (maxBottom, pow) |> D42 |> run
let D42Run = D42RunSpecial (10, 8)
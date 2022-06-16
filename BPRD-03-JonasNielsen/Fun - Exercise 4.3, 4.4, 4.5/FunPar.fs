// Implementation file for parser generated by fsyacc
module FunPar
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "FunPar.fsy"

 (* File Fun/FunPar.fsy 
    Parser for micro-ML, a small functional language; one-argument functions.
    sestoft@itu.dk * 2009-10-19
  *)

 open Absyn;

# 15 "FunPar.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | AND
  | OR
  | LPAR
  | RPAR
  | EQ
  | NE
  | GT
  | LT
  | GE
  | LE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | ELSE
  | END
  | FALSE
  | IF
  | IN
  | LET
  | NOT
  | THEN
  | TRUE
  | CSTBOOL of (bool)
  | NAME of (string)
  | CSTINT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EQ
    | TOKEN_NE
    | TOKEN_GT
    | TOKEN_LT
    | TOKEN_GE
    | TOKEN_LE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_ELSE
    | TOKEN_END
    | TOKEN_FALSE
    | TOKEN_IF
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_NOT
    | TOKEN_THEN
    | TOKEN_TRUE
    | TOKEN_CSTBOOL
    | TOKEN_NAME
    | TOKEN_CSTINT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Expr
    | NONTERM_NAMELIST
    | NONTERM_AtExpr
    | NONTERM_AppExprList
    | NONTERM_AppExpr
    | NONTERM_Const

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | AND  -> 1 
  | OR  -> 2 
  | LPAR  -> 3 
  | RPAR  -> 4 
  | EQ  -> 5 
  | NE  -> 6 
  | GT  -> 7 
  | LT  -> 8 
  | GE  -> 9 
  | LE  -> 10 
  | PLUS  -> 11 
  | MINUS  -> 12 
  | TIMES  -> 13 
  | DIV  -> 14 
  | MOD  -> 15 
  | ELSE  -> 16 
  | END  -> 17 
  | FALSE  -> 18 
  | IF  -> 19 
  | IN  -> 20 
  | LET  -> 21 
  | NOT  -> 22 
  | THEN  -> 23 
  | TRUE  -> 24 
  | CSTBOOL _ -> 25 
  | NAME _ -> 26 
  | CSTINT _ -> 27 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_AND 
  | 2 -> TOKEN_OR 
  | 3 -> TOKEN_LPAR 
  | 4 -> TOKEN_RPAR 
  | 5 -> TOKEN_EQ 
  | 6 -> TOKEN_NE 
  | 7 -> TOKEN_GT 
  | 8 -> TOKEN_LT 
  | 9 -> TOKEN_GE 
  | 10 -> TOKEN_LE 
  | 11 -> TOKEN_PLUS 
  | 12 -> TOKEN_MINUS 
  | 13 -> TOKEN_TIMES 
  | 14 -> TOKEN_DIV 
  | 15 -> TOKEN_MOD 
  | 16 -> TOKEN_ELSE 
  | 17 -> TOKEN_END 
  | 18 -> TOKEN_FALSE 
  | 19 -> TOKEN_IF 
  | 20 -> TOKEN_IN 
  | 21 -> TOKEN_LET 
  | 22 -> TOKEN_NOT 
  | 23 -> TOKEN_THEN 
  | 24 -> TOKEN_TRUE 
  | 25 -> TOKEN_CSTBOOL 
  | 26 -> TOKEN_NAME 
  | 27 -> TOKEN_CSTINT 
  | 30 -> TOKEN_end_of_input
  | 28 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM_Main 
    | 2 -> NONTERM_Expr 
    | 3 -> NONTERM_Expr 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_Expr 
    | 6 -> NONTERM_Expr 
    | 7 -> NONTERM_Expr 
    | 8 -> NONTERM_Expr 
    | 9 -> NONTERM_Expr 
    | 10 -> NONTERM_Expr 
    | 11 -> NONTERM_Expr 
    | 12 -> NONTERM_Expr 
    | 13 -> NONTERM_Expr 
    | 14 -> NONTERM_Expr 
    | 15 -> NONTERM_Expr 
    | 16 -> NONTERM_Expr 
    | 17 -> NONTERM_Expr 
    | 18 -> NONTERM_Expr 
    | 19 -> NONTERM_NAMELIST 
    | 20 -> NONTERM_NAMELIST 
    | 21 -> NONTERM_AtExpr 
    | 22 -> NONTERM_AtExpr 
    | 23 -> NONTERM_AtExpr 
    | 24 -> NONTERM_AtExpr 
    | 25 -> NONTERM_AtExpr 
    | 26 -> NONTERM_AppExprList 
    | 27 -> NONTERM_AppExprList 
    | 28 -> NONTERM_AppExpr 
    | 29 -> NONTERM_AppExpr 
    | 30 -> NONTERM_Const 
    | 31 -> NONTERM_Const 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 30 
let _fsyacc_tagOfErrorTerminal = 28

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | AND  -> "AND" 
  | OR  -> "OR" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | EQ  -> "EQ" 
  | NE  -> "NE" 
  | GT  -> "GT" 
  | LT  -> "LT" 
  | GE  -> "GE" 
  | LE  -> "LE" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | MOD  -> "MOD" 
  | ELSE  -> "ELSE" 
  | END  -> "END" 
  | FALSE  -> "FALSE" 
  | IF  -> "IF" 
  | IN  -> "IN" 
  | LET  -> "LET" 
  | NOT  -> "NOT" 
  | THEN  -> "THEN" 
  | TRUE  -> "TRUE" 
  | CSTBOOL _ -> "CSTBOOL" 
  | NAME _ -> "NAME" 
  | CSTINT _ -> "CSTINT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | NE  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | GE  -> (null : System.Object) 
  | LE  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MOD  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | IN  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | CSTBOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CSTINT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 23us; 65535us; 0us; 2us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 32us; 14us; 33us; 15us; 34us; 16us; 35us; 17us; 36us; 18us; 37us; 19us; 38us; 20us; 39us; 21us; 40us; 22us; 41us; 23us; 42us; 24us; 43us; 25us; 44us; 26us; 51us; 27us; 52us; 28us; 55us; 29us; 56us; 30us; 58us; 31us; 2us; 65535us; 45us; 46us; 50us; 54us; 26us; 65535us; 0us; 4us; 4us; 60us; 5us; 60us; 6us; 4us; 8us; 4us; 10us; 4us; 12us; 4us; 32us; 4us; 33us; 4us; 34us; 4us; 35us; 4us; 36us; 4us; 37us; 4us; 38us; 4us; 39us; 4us; 40us; 4us; 41us; 4us; 42us; 4us; 43us; 4us; 44us; 4us; 51us; 4us; 52us; 4us; 55us; 4us; 56us; 4us; 58us; 4us; 60us; 60us; 3us; 65535us; 4us; 62us; 5us; 63us; 60us; 61us; 23us; 65535us; 0us; 5us; 6us; 5us; 8us; 5us; 10us; 5us; 12us; 5us; 32us; 5us; 33us; 5us; 34us; 5us; 35us; 5us; 36us; 5us; 37us; 5us; 38us; 5us; 39us; 5us; 40us; 5us; 41us; 5us; 42us; 5us; 43us; 5us; 44us; 5us; 51us; 5us; 52us; 5us; 55us; 5us; 56us; 5us; 58us; 5us; 26us; 65535us; 0us; 47us; 4us; 47us; 5us; 47us; 6us; 47us; 8us; 47us; 10us; 47us; 12us; 47us; 32us; 47us; 33us; 47us; 34us; 47us; 35us; 47us; 36us; 47us; 37us; 47us; 38us; 47us; 39us; 47us; 40us; 47us; 41us; 47us; 42us; 47us; 43us; 47us; 44us; 47us; 51us; 47us; 52us; 47us; 55us; 47us; 56us; 47us; 58us; 47us; 60us; 47us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 27us; 30us; 57us; 61us; 85us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 14us; 1us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 1us; 1us; 2us; 2us; 28us; 2us; 3us; 29us; 1us; 4us; 14us; 4us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 1us; 4us; 14us; 4us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 1us; 4us; 14us; 4us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 1us; 5us; 14us; 5us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 23us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 23us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 24us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 24us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 25us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 10us; 1us; 11us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 2us; 19us; 20us; 1us; 20us; 1us; 21us; 1us; 22us; 2us; 23us; 24us; 2us; 23us; 24us; 1us; 23us; 1us; 23us; 1us; 23us; 1us; 24us; 1us; 24us; 1us; 24us; 1us; 24us; 1us; 25us; 1us; 25us; 2us; 26us; 27us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 31us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 19us; 21us; 24us; 27us; 29us; 44us; 46us; 61us; 63us; 78us; 80us; 95us; 110us; 125us; 140us; 155us; 170us; 185us; 200us; 215us; 230us; 245us; 260us; 275us; 290us; 305us; 320us; 335us; 350us; 365us; 367us; 369us; 371us; 373us; 375us; 377us; 379us; 381us; 383us; 385us; 387us; 389us; 391us; 394us; 396us; 398us; 400us; 403us; 406us; 408us; 410us; 412us; 414us; 416us; 418us; 420us; 422us; 424us; 427us; 429us; 431us; 433us; 435us; |]
let _fsyacc_action_rows = 66
let _fsyacc_actionTableElements = [|7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 0us; 49152us; 14us; 32768us; 0us; 3us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 0us; 16385us; 5us; 16386us; 3us; 58us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 5us; 16387us; 3us; 58us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 23us; 8us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 16us; 10us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 13us; 16388us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 5us; 16389us; 1us; 43us; 2us; 44us; 13us; 34us; 14us; 35us; 15us; 36us; 5us; 16390us; 1us; 43us; 2us; 44us; 13us; 34us; 14us; 35us; 15us; 36us; 5us; 16391us; 1us; 43us; 2us; 44us; 13us; 34us; 14us; 35us; 15us; 36us; 2us; 16392us; 1us; 43us; 2us; 44us; 2us; 16393us; 1us; 43us; 2us; 44us; 2us; 16394us; 1us; 43us; 2us; 44us; 11us; 16395us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16396us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16397us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16398us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16399us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16400us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 13us; 16401us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 13us; 16402us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 20us; 52us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 17us; 53us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 20us; 56us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 17us; 57us; 14us; 32768us; 1us; 43us; 2us; 44us; 4us; 59us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 1us; 16403us; 26us; 45us; 0us; 16404us; 0us; 16405us; 0us; 16406us; 1us; 32768us; 26us; 50us; 2us; 32768us; 5us; 51us; 26us; 45us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 0us; 16407us; 1us; 32768us; 5us; 55us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 0us; 16408us; 7us; 32768us; 3us; 58us; 12us; 12us; 19us; 6us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 0us; 16409us; 5us; 16410us; 3us; 58us; 21us; 49us; 25us; 65us; 26us; 48us; 27us; 64us; 0us; 16411us; 0us; 16412us; 0us; 16413us; 0us; 16414us; 0us; 16415us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 8us; 9us; 24us; 25us; 31us; 37us; 45us; 60us; 68us; 83us; 91us; 105us; 113us; 119us; 125us; 131us; 134us; 137us; 140us; 152us; 164us; 176us; 188us; 200us; 212us; 226us; 240us; 255us; 270us; 285us; 300us; 315us; 323us; 331us; 339us; 347us; 355us; 363us; 371us; 379us; 387us; 395us; 403us; 411us; 419us; 421us; 422us; 423us; 424us; 426us; 429us; 437us; 445us; 446us; 448us; 456us; 464us; 465us; 473us; 474us; 480us; 481us; 482us; 483us; 484us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 6us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; 2us; 1us; 1us; 7us; 8us; 3us; 1us; 2us; 2us; 2us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; 6us; 6us; 7us; 7us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16404us; 16405us; 16406us; 65535us; 65535us; 65535us; 65535us; 16407us; 65535us; 65535us; 65535us; 16408us; 65535us; 16409us; 65535us; 16411us; 16412us; 16413us; 16414us; 16415us; |]
let _fsyacc_reductions ()  =    [| 
# 270 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 279 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "FunPar.fsy"
                                                               _1 
                   )
# 37 "FunPar.fsy"
                 : Absyn.expr));
# 290 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "FunPar.fsy"
                                                               _1                     
                   )
# 41 "FunPar.fsy"
                 : Absyn.expr));
# 301 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "FunPar.fsy"
                                                               _1                     
                   )
# 42 "FunPar.fsy"
                 : Absyn.expr));
# 312 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "FunPar.fsy"
                                                               If(_2, _4, _6)         
                   )
# 43 "FunPar.fsy"
                 : Absyn.expr));
# 325 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "FunPar.fsy"
                                                               Prim("-", CstI 0, _2)  
                   )
# 44 "FunPar.fsy"
                 : Absyn.expr));
# 336 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "FunPar.fsy"
                                                               Prim("+",  _1, _3)     
                   )
# 45 "FunPar.fsy"
                 : Absyn.expr));
# 348 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "FunPar.fsy"
                                                               Prim("-",  _1, _3)     
                   )
# 46 "FunPar.fsy"
                 : Absyn.expr));
# 360 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "FunPar.fsy"
                                                               Prim("*",  _1, _3)     
                   )
# 47 "FunPar.fsy"
                 : Absyn.expr));
# 372 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "FunPar.fsy"
                                                               Prim("/",  _1, _3)     
                   )
# 48 "FunPar.fsy"
                 : Absyn.expr));
# 384 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "FunPar.fsy"
                                                               Prim("%",  _1, _3)     
                   )
# 49 "FunPar.fsy"
                 : Absyn.expr));
# 396 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "FunPar.fsy"
                                                               Prim("=",  _1, _3)     
                   )
# 50 "FunPar.fsy"
                 : Absyn.expr));
# 408 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "FunPar.fsy"
                                                               Prim("<>", _1, _3)     
                   )
# 51 "FunPar.fsy"
                 : Absyn.expr));
# 420 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "FunPar.fsy"
                                                               Prim(">",  _1, _3)     
                   )
# 52 "FunPar.fsy"
                 : Absyn.expr));
# 432 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "FunPar.fsy"
                                                               Prim("<",  _1, _3)     
                   )
# 53 "FunPar.fsy"
                 : Absyn.expr));
# 444 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "FunPar.fsy"
                                                               Prim(">=", _1, _3)     
                   )
# 54 "FunPar.fsy"
                 : Absyn.expr));
# 456 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "FunPar.fsy"
                                                               Prim("<=", _1, _3)     
                   )
# 55 "FunPar.fsy"
                 : Absyn.expr));
# 468 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "FunPar.fsy"
                                              If(_1, _3, CstB false) 
                   )
# 56 "FunPar.fsy"
                 : Absyn.expr));
# 480 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "FunPar.fsy"
                                             If(_1, CstB true, _3 ) 
                   )
# 57 "FunPar.fsy"
                 : Absyn.expr));
# 492 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "FunPar.fsy"
                                [_1]
                   )
# 62 "FunPar.fsy"
                 : string list));
# 503 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "FunPar.fsy"
                                         _1::_2
                   )
# 63 "FunPar.fsy"
                 : string list));
# 515 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "FunPar.fsy"
                                                               _1                     
                   )
# 67 "FunPar.fsy"
                 : Absyn.expr));
# 526 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "FunPar.fsy"
                                                               Var _1                 
                   )
# 68 "FunPar.fsy"
                 : Absyn.expr));
# 537 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "FunPar.fsy"
                                                               Let(_2, _4, _6)        
                   )
# 69 "FunPar.fsy"
                 : Absyn.expr));
# 550 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string list)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "FunPar.fsy"
                                                                Letfun(_2, _3, _5, _7) 
                   )
# 70 "FunPar.fsy"
                 : Absyn.expr));
# 564 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "FunPar.fsy"
                                                               _2                     
                   )
# 71 "FunPar.fsy"
                 : Absyn.expr));
# 575 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "FunPar.fsy"
                                      [_1]
                   )
# 75 "FunPar.fsy"
                 : Absyn.expr list));
# 586 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "FunPar.fsy"
                                               _1::_2
                   )
# 76 "FunPar.fsy"
                 : Absyn.expr list));
# 598 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "FunPar.fsy"
                                                                    Call(_1, _2)           
                   )
# 79 "FunPar.fsy"
                 : Absyn.expr));
# 610 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "FunPar.fsy"
                                                                    Call(_1, _2)           
                   )
# 80 "FunPar.fsy"
                 : Absyn.expr));
# 622 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "FunPar.fsy"
                                                               CstI(_1)               
                   )
# 84 "FunPar.fsy"
                 : Absyn.expr));
# 633 "FunPar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "FunPar.fsy"
                                                               CstB(_1)               
                   )
# 85 "FunPar.fsy"
                 : Absyn.expr));
|]
# 645 "FunPar.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 31;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Absyn.expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))

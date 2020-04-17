module Ass7

open System
open JParsec
open TextParser
open Parser

type aExp =
    | N of int              (* Integer literal *)
    | V of string           (* Variable reference *)
        
    | WL                    (* Word length *)
    | PV of aExp            (* Point value lookup at word index *)
        
    | Add of aExp * aExp    (* Addition *)
    | Sub of aExp * aExp    (* Subtraction *)
    | Mul of aExp * aExp    (* Multiplication *)
    | Div of aExp * aExp    (* Division *)
    | Mod of aExp * aExp    (* Modulo *)

    | CharToInt of cExp     (* Cast to integer *)

and cExp =
    | C  of char             (* Character literal *)
    | CV of aExp             (* Character lookup at word index *)
       
    | ToUpper of cExp        (* Convert character to upper case *)
    | ToLower of cExp        (* Convert character to lower case *)
       
    | IntToChar of aExp      (* Cast to character *)

type bExp =             
    | TT                   (* True *)
    | FF                   (* False *)
        
    | AEq of aExp * aExp   (* Numeric equality *)
    | ALt of aExp * aExp   (* Numeric less than *)
        
    | Not of bExp          (* Boolean not *)
    | Conj of bExp * bExp  (* Boolean conjunction *)
        
    | IsVowel of cExp      (* Check for vowel *)
    | IsConsonant of cExp  (* Check for constant *)

type stm =                
    | Declare of string       (* NEW: Variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* Nop *)
    | Seq of stm * stm        (* Sequential composition *)
    | ITE of bExp * stm * stm (* If-Then-Else statement *)
    | While of bExp * stm     (* While statement *)

module ImpParser =
    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)
        
    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)

    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)                (* numeric inequality *)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric smaller than or equal to *)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)


    let parenthesise p = failwith "not implemented"
    let binop p = failwith "not implemented"

    let TermParse, tref = createParserForwardedToRef<TextInputState, aExp>()
    let ProdParse, pref = createParserForwardedToRef<TextInputState, aExp>()
    let AtomParse, aref = createParserForwardedToRef<TextInputState, aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let AExpParse = TermParse 


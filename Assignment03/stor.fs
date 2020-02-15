﻿// Learn more about F# at http://fsharp.org

open System
type aExp =   
| N of int            // Integer value   
| V of string        // Variable   
| WL                  // Length of the word   
| PV of aExp          // Point value of character at specific word index   
| Add of aExp * aExp  // Addition   
| Sub of aExp * aExp  // Subtraction   
| Mul of aExp * aExp  // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let rec arithEvalSimple (exp:aExp) : int = 
    match exp with
    |N x -> x
    |Add(x,y) -> arithEvalSimple x + arithEvalSimple y
    |Sub(x,y) -> arithEvalSimple x - arithEvalSimple y
    |Mul(x,y) -> arithEvalSimple x * arithEvalSimple y

let rec arithEvalState (exp:aExp) (state:Map<string, int>) : int =
    match exp with
    |V s when (state.TryFind s).IsSome -> state.[s]
    |N x -> x
    |Add(x,y) -> (arithEvalState x state) + arithEvalState y state
    |Sub(x,y) -> arithEvalState x state - arithEvalState y state
    |Mul(x,y) -> arithEvalState x state * arithEvalState y state

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

type word = (char * int) list

let hello:word = ('H',4)::('E',1)::('L',1)::('L',1)::[('O',2)];; 

let rec arithEval (exp:aExp) (word:word) (state:Map<string,int>) : int =
    match exp with
    |V s -> if (state.TryFind s).IsSome then state.[s] else 0
    |N x -> x
    |WL -> word.Length
    |PV aexp -> snd (word.[(arithEval aexp word state)]) 
    |Add(x,y) -> arithEval x word state + arithEval y word state
    |Sub(x,y) -> arithEval x word state - arithEval y word state
    |Mul(x,y) -> arithEval x word state * arithEval y word state
    |_ -> 0
    
type cExp =
   | C  of char      (* Character value *)
   | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
   | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
   | CV of aExp      (* Character lookup at word index *)

let rec charEval (exp:cExp) (word:word) (state:Map<string,int>) :char =
    match exp with
    |C exp -> exp
    |ToUpper exp -> System.Char.ToUpper (charEval exp word state)
    |ToLower exp -> System.Char.ToLower (charEval exp word state)
    |CV exp -> fst word.[(arithEval exp word state)] 
    
type bExp =                
| TT                  (* true *)   
| FF                  (* false *)   
| AEq of aExp * aExp  (* numeric equality *)   
| ALt of aExp * aExp  (* numeric less than *)   
| Not of bExp         (* boolean not *)   
| Conj of bExp * bExp (* boolean conjunction *)   
| IsVowel of cExp     (* check for vowel *)   
| IsConsonant of cExp (* check for constant *) 

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
let (.=.) a b = AEq (a, b)   
let (.<.) a b = ALt (a, b)   
let (.<>.) a b = ~~(a .=. b)                (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let rec boolEval (exp:bExp) (word:word) (state:Map<string,int>) : bool =
    match exp with
    |TT -> true
    |FF -> false 
    |AEq (x,y) -> (arithEval x word state) = (arithEval y word state)
    |ALt (x,y) ->(arithEval x word state) >= (arithEval y word state)
    |Not exp -> not (boolEval exp word state)
    

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

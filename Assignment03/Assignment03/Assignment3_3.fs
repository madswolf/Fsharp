﻿
module aExp
    open System
    (* Exercise 3.3 *)

    type word = (char * int) list
    type squareFun = word -> int -> int -> int
    
    let hello:word = [('H',4);('E',1);('L',1);('L',1);('O',2)]
    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)

    let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
    let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
    let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

    let arithDoubleWordScore = N 2 .*. V "_acc_"
    let arithTripleWordScore = N 3 .*. V "_acc_"

    let rec arithEval (exp:aExp) (word:word) (state:Map<string, int>) : int =
        match exp with
        |V s -> if (state.TryFind s).IsSome then state.[s] else 0
        |N x -> x
        |WL -> word.Length
        |PV aexp -> snd (word.[(arithEval aexp word state)]) 
        |Add(x,y) -> arithEval x word state + arithEval y word state
        |Sub(x,y) -> arithEval x word state - arithEval y word state
        |Mul(x,y) -> arithEval x word state * arithEval y word state

    (* Exercise 3.4 *)

    type cExp =
       | C  of char      (* Character value *)
       | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
       | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
       | CV of aExp      (* Character lookup at word index *)

    let rec charEval (exp:cExp) (word:word) (state:Map<string, int>) : char =
        match exp with
        |C exp -> exp
        |ToUpper exp -> System.Char.ToUpper (charEval exp word state)
        |ToLower exp -> System.Char.ToLower (charEval exp word state)
        |CV exp -> fst word.[(arithEval exp word state)] 

    (* Exercise 3.5 *)

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp   (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

    let isVowel c =
        match System.Char.ToLower c with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false
    
    let isConsonant c = 
        if System.Char.IsLetter c then 
            if isVowel c then false else true
        else false

    let rec boolEval (exp:bExp) (word:word) (state:Map<string,int>) : bool =
        match exp with
        |TT -> true
        |FF -> false 
        |AEq (x,y) -> (arithEval x word state) = (arithEval y word state)
        |ALt (x,y) ->(arithEval x word state) < (arithEval y word state)
        |Not exp -> not (boolEval exp word state)
        |Conj (x,y) -> (boolEval x word state) && (boolEval y word state)
        |IsVowel exp -> isVowel (charEval exp word state)
        |IsConsonant exp -> isConsonant (charEval exp word state)

    (* Exercise 3.6 *)

    type stmnt =
       | Skip                        (* does nothing *)
       | Ass of string * aExp        (* variable assignment *)
       | Seq of stmnt * stmnt        (* sequential composition *)
       | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
       | While of bExp * stmnt       (* while statement *)

    let rec evalStmnt (stmnt:stmnt) (word:word) (state:Map<string, int>) : Map<string, int> =
        match stmnt with
        |Skip -> state
        |Ass (x,a) -> state.Add(x,arithEval a word state)
        |Seq (stm1,stm2) -> evalStmnt stm2 word (evalStmnt stm1 word state)
        |ITE (guard,stm1,stm2) -> 
            if (boolEval guard word state) 
                then evalStmnt stm1 word state 
                else evalStmnt stm2 word state
        |While (guard, stm) -> 
            if (boolEval guard word state)
                then evalStmnt (While (guard, stm)) word (evalStmnt stm word state)
                else state

    (* Exercise 3.7 *)


    let stmnt2SquareFun (stm:stmnt)  :squareFun =
        fun (word:word) (pos:int) (acc:int) -> 
        let state = evalStmnt stm word (Map.empty.Add("_pos_", pos).Add("_acc_", acc))
        state.["_result_"] 

    let singleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithSingleLetterScore))
    let doubleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleLetterScore))
    let tripleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleLetterScore))

    let doubleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleWordScore))
    let tripleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleWordScore))

    let oddConsonants : squareFun = 
        stmnt2SquareFun 
            (Seq (Ass ("_result_", V "_acc_"),
                  While (V "i" .<. WL,
                         Seq(
                             ITE (IsConsonant (CV (V "i")),
                                  Ass ("_result_", V "_result_" .*. N -1),
                                  Skip),
                             Ass ("i", V "i" .+. N 1)))))

    (* Exercise 3.8 *)

    type square = (int * squareFun) list
    type square2 = (int * stmnt) list

    let calculatePoints  (squares:square list)  (word:word)=
        let thing = List.mapi (fun index square ->  List.map (fun (innersquare:int * squareFun) ->  (fst innersquare,snd innersquare word index) ) square) squares|>
                    List.fold (fun a b -> a @ b) [] |>
                    List.sortBy(fun square -> fst square) |>
                    List.map (fun square -> snd square) |>
                    List.fold(fun acumulator square -> square << acumulator) (fun x -> x) 
        thing 0

    
    let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
    let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
    let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]

    let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
    let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

    let calculatePoints2 (squares:square2 list) (word:word) : int = 
        List.map (fun innersquares -> List.map (fun square2 -> (fst square2,snd square2 |> stmnt2SquareFun)) innersquares) squares |>
        calculatePoints <|word 

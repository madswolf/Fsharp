module aExp

    (* Exercise 3.3 *)

    type word = (char * int) list
    type squareFun = word -> int -> int -> int

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


    let rec boolEval bExp = failwith "not implemented"

    type stmnt =
       | Skip                        (* does nothing *)
       | Ass of string * aExp        (* variable assignment *)
       | Seq of stmnt * stmnt        (* sequential composition *)
       | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
       | While of bExp * stmnt       (* while statement *)

    let rec evalStmnt stm = failwith "not implemented"

    let stmnt2SquareFun stm = failwith "not imlpemented"

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

    type square2 = (int * stmnt) list

    let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
    let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
    let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]

    let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
    let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

    let calculatePoints2 : square2 list -> word -> int = failwith "not implemented"
    
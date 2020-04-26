module Eval
    open FParsec   
    open StateMonad
    open Ass7
    (* Code for testing *)

    let hello = ('H',4)::('E',1)::('L',1)::('L',1)::[('O',1)]  
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a:SM<int>) (b:SM<int>) : SM<int> = 
        a >>= fun x -> 
        b >>= fun y->
        ret (x + y)
    
    let div(a:SM<int>) (b:SM<int>) : SM<int> =  
        a >>= fun x -> 
        b >>= fun y->
        if y <> 0 then ret (x / y) else fail DivisionByZero  

    let isVowel c =
        match System.Char.ToLower c with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false
    
    let isConsonant c = 
        if System.Char.IsLetter c then 
            if isVowel c then false else true
        else false
    
    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f a b =
        a >>= fun x ->     
        b >>= fun y ->     
        ret (f x y)

    let binopWithError f a b c e =  
           a >>= fun x -> 
           b >>= fun y->
           if c y  then ret (f x y) else fail e

    let rec arithEval (a:aExp) : SM<int> =
        match a with
        |N exp -> ret exp
        |V exp -> lookup exp
        |WL    -> wordLength
        |PV exp   -> arithEval exp >>= pointValue
        |Add(x,y) -> binop (+) (arithEval x) (arithEval y)
        |Sub(x,y) -> binop (-) (arithEval x) (arithEval y)
        |Mul(x,y) -> binop ( * ) (arithEval x) (arithEval y)
        |Div(x,y) -> binopWithError (/) (arithEval x) (arithEval y) ((<>) 0) DivisionByZero
        |Mod(x,y) -> binopWithError (%) (arithEval x) (arithEval y) ((<>) 0) DivisionByZero
        |CharToInt cExp -> charEval cExp >>= (int >> ret)
    and
        charEval (c:cExp) : SM<char> = 
        match c with
        |C cExp -> ret cExp
        |CV aExp -> (arithEval aExp) >>= characterValue
        |ToUpper cExp -> charEval cExp >>= (System.Char.ToUpper >> ret)
        |ToLower cExp -> charEval cExp >>= (System.Char.ToLower >> ret)
        |IntToChar aExp ->  arithEval aExp >>= (char >> ret)
    and
        boolEval b : SM<bool> = 
        match b with
        |TT -> ret true
        |FF -> ret false
        |AEq(x,y) -> binop (=) (arithEval x) (arithEval y)
        |ALt(x,y) -> binop (<) (arithEval x) (arithEval y)
        |Not bExp -> boolEval bExp >>= (not >> ret)
        |Conj(x,y) -> binop (&&) (boolEval x) (boolEval y)
        |IsVowel bExp -> charEval bExp >>= (isVowel >> ret)
        |IsConsonant bExp -> charEval bExp >>= (isConsonant >> ret)

    let rec stmntEval (stmnt:stm) : SM<unit> = 
        match stmnt with
        |Declare string -> declare string 
        |Ass(string,aExp) -> (arithEval aExp) >>= (update string)
        |Skip -> ret ()
        |Seq(x,y) -> stmntEval x >>>= stmntEval y
        |ITE(bExp,x,y) ->  push>>>=(boolEval bExp)>>= (fun bool -> stmntEval (if bool then x else y))>>=(fun _ -> pop)
        |While(bExp, stm) -> (stmntEval (ITE(bExp,Seq(stm,While(bExp,stm)),Skip)))
        

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let binop2 f a b = 
        prog{
            let! x = a
            let! y = b
            return f x y
        }

    let binop2WithError f a b c e =
        prog{
            let! x = a
            let! y = b
            if c y then return f x y
            else return! fail e
        }

    let rec arithEval2 a = 
        match a with
        |N aExp -> prog{return aExp}
        |V aExp -> 
            prog{
                let! x = lookup aExp
                return x
            }
        |WL     -> 
            prog{
                let! x = wordLength
                return x
            }
        |PV aExp -> 
            prog{
                let! x = arithEval2 aExp
                let! y = pointValue x
                return y
            }
        |Add(x,y) -> binop2 (+) (arithEval x) (arithEval y)
        |Sub(x,y) -> binop2 (-) (arithEval x) (arithEval y)
        |Mul(x,y) -> binop2 ( * ) (arithEval x) (arithEval y)
        |Div(x,y) -> binop2WithError (/) (arithEval x) (arithEval y) ((<>) 0) DivisionByZero
        |Mod(x,y) -> binop2WithError (%) (arithEval x) (arithEval y) ((<>) 0) DivisionByZero
        |CharToInt cExp -> 
            prog{
                let! x = charEval2 cExp
                return int x
            }
    and
        charEval2 c = 
        match c with
        |C cExp -> prog{return cExp}
        |CV aExp -> 
            prog{
                let! x = arithEval2 aExp
                let! y = characterValue x
                return y
            }
        |ToUpper cExp -> 
            prog{
                let! x = charEval2 cExp
                return System.Char.ToUpper x
            }
        |ToLower cExp -> 
            prog{
                let! x = charEval2 cExp
                return System.Char.ToLower x
            }
        |IntToChar aExp -> 
            prog{
                let! x = arithEval2 aExp
                return char x
            }
    and
        boolEval2 b = 
        match b with
        |TT -> prog{return true}
        |FF -> prog{return false}
        |AEq(x,y) -> binop (=) (arithEval2 x) (arithEval2 y)
        |ALt(x,y) -> binop (<) (arithEval2 x) (arithEval2 y)
        |Not bExp -> 
            prog{
                let! x = boolEval2 bExp
                return not x
                }
        |Conj(x,y) -> binop (&&) (boolEval2 x) (boolEval2 y)
        |IsVowel cExp -> 
            prog{
                let! x = charEval2 cExp
                return isVowel x
            } 
        |IsConsonant cExp -> 
            prog{
                let! x = charEval2 cExp
                return isConsonant x
            }

    let rec stmntEval2 stm = 
        match stm with
        |Declare string -> 
            prog{
                do! declare string
            }
        |Ass(string,aExp) -> 
            prog{
                let! x = arithEval2 aExp
                do! update string x
            }
        |Skip -> prog{return ()}
        |Seq(x,y) ->
            prog{
                do! stmntEval2 x
                do! stmntEval2 y
            }
        |ITE(bExp, x, y) -> 
            prog{
                do! push
                let! bool = boolEval2 bExp
                if bool 
                then 
                    let! a = stmntEval2 x
                    return a
                else
                    let! b = stmntEval2 y
                    return b
            }
        |While(bExp, stm) -> stmntEval2 (ITE(bExp,Seq(stm,While(bExp,stm)),Skip))
        
(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> int
    
    let stmntToSquareFun stm :squareFun = 
        fun word pos acc ->
            let state = mkState [("_pos_", pos);("_acc_", acc);("_result_",0)] word ["_pos_";"_acc_";"_result_"] 
            (stmntEval2 stm)>>>= (lookup "_result_") |> evalSM state |>
            function 
            |Success v -> v
            |_ -> failwith "uwu"

    type coord = int * int

    type boardFun = coord -> int

    let stmntToBoardFun stm (squares:Map<int,Map<int,squareFun>>): boardFun = 
        fun (x,y) ->
            let state = mkState [("_x_", x);("_y_", y);("_result_",0)] [] ["_x_";"_y_";"_result_"] 
            (stmntEval2 stm)>>>= (lookup "_result_")|> evalSM state |>
            function 
            |Success v -> v
            |_ -> failwith "uwu"

            
    (*let stmntToBoardFunAlt stm (squares:Map<int,Map<int,squareFun>>) : boardFun =
        fun (x,y)->
            let state = mkState [("_x_", x);("_y_", y);("_result_",0)] [] ["_x_";"_y_";"_result_"]
            prog{
                do! stmntEval2 stm
                let! id = lookup "_result_"
                return squares.TryFind id
            } |> evalSM state |>
            function 
            |Success v -> v
            |_ -> failwith "uwu"*)

    (*type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    //didn't bother fixing after changing boardFun
    let mkBoard center defaultSq boardStmnt ids : board = 
        let map = List.fold(fun acc item -> Map.add (fst item) (stmntToSquareFun (snd item)) acc) Map.empty ids
        let defaultSquare = stmntToSquareFun defaultSq 
        let squares = stmntToBoardFun boardStmnt map
        {center= center; defaultSquare= defaultSquare; squares= squares}*)
    
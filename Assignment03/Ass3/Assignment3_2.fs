module AExpState

    (* Exercise 3.2 *)

    type aExp =
        | N of int
        | V of string
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)


    type state = Map<string, int>

    let rec arithEvalState (exp:aExp) (state:Map<string, int>) : int =
        match exp with
        |V s when (state.TryFind s).IsSome -> state.[s]
        |N x -> x
        |Add(x,y) -> (arithEvalState x state) + arithEvalState y state
        |Sub(x,y) -> arithEvalState x state - arithEvalState y state
        |Mul(x,y) -> arithEvalState x state * arithEvalState y state
        |_ -> 0
    
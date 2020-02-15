module AExpSimple

    (* Exercise 3.1 *)

    type aExp =
        | N of int
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)

    let rec arithEvalSimple (exp:aExp) : int =
        match exp with
        |N x -> x
        |Add(x,y) -> arithEvalSimple x + arithEvalSimple y
        |Sub(x,y) -> arithEvalSimple x - arithEvalSimple y
        |Mul(x,y) -> arithEvalSimple x * arithEvalSimple y

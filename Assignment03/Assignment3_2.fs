module Ass3_2

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

    let rec arithEvalState aExp = failwith "not implemented"
    
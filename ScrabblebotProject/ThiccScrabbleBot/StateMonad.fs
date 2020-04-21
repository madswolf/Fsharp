﻿module StateMonad
    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string
        | EmptyStack

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = S(fun s -> Success ((), {s with vars = s.vars.Tail}))      

    let wordLength : SM<int> = S(fun s -> Success(s.word.Length,s))      

    let characterValue (pos : int) : SM<char> = S(fun s -> 
        match s.word.Length with
        |length when length > pos -> Success(fst s.word.[pos],s)
        |_ -> Failure(IndexOutOfBounds pos))


    let pointValue (pos : int) : SM<int> = S(fun s -> 
        match s.word.Length with
        |length when length > pos && pos >= 0 -> Success(snd s.word.[pos],s)
        |_ -> Failure(IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        S(fun s -> 
            match var with
            | var when s.reserved.Contains var -> Failure(ReservedName var)
            | var -> 
                match s.vars with
                |[] -> Failure(EmptyStack)
                |m::ms -> 
                    match Map.tryFind var m with
                    |Some v -> Failure(VarExists var)
                    |None   -> Success((), {s with vars = (Map.add var 0 m)::ms})
            
        )
            
    let update (var : string) (value : int) : SM<unit> = 
        let rec aux =
            function
            | [] -> None
            |m::ms -> 
                match Map.tryFind var m with
                |Some _ -> Some ((Map.add var value m)::ms)
                |None   -> 
                    match aux ms with
                    |Some map -> Some (m::map)
                    |None -> None
        S(fun s ->
            match aux (s.vars) with
            |Some m -> Success ((),{s with vars = m})
            |None   -> Failure (VarNotFound var)
        )      
              

    
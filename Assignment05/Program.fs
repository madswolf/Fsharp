// Learn more about F# at http://fsharp.org

open System

let sum (m:int) (n:int) : int =
    let rec sumA (innerN:int) (acc:int) :int =
        match innerN with 
        |0 -> acc + m 
        |innerN -> sumA (innerN - 1) (innerN + m + acc)
    sumA n 0    

let length (list: 'a list) : int =
    let rec lengthA (list: 'a list) (acc:int) :int =
        match list with
        |[] -> acc
        |x::xs -> lengthA xs (acc + 1) 
    lengthA list 0

let foldBack (folder: 'a -> 'b -> 'b ) (list: 'a list) (acc:'b) :'b =
    let rec foldBackC (innerList: 'a list) (continuation: 'b -> 'b): 'b =
        match innerList with
        | [] -> continuation acc
        | x::xs -> foldBackC xs (fun innerAcc -> continuation(folder x innerAcc)) 
    foldBackC list id

let fact (n:int) :int =
    let rec factC (innerN:int) (continuation: int -> int) : int =
        match innerN with 
        |1 -> continuation 1  
        |innerN -> factC (innerN-1) (fun x -> innerN * x)
    factC n id

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x

//this is an unusual recursion where it goes up instead of down, but it seemed convoluted to go the other way
let fibA (x:int) :int =
    let rec aux (nminus1:int) (nminus2:int) (n:int) :int =
        match n with   
        |0 -> nminus1
        |n when n = x -> aux 1 0 (n-1)
        |n -> aux (nminus1+nminus2) nminus1 (n-1)
    aux 0 0 x

let fibC (x:int) :int =
    let rec aux n continuation = 
        match n with 
        | 1 -> continuation 1 
        | 2 -> continuation 1 
        | _ ->  aux (n-1) (fun nminus1 -> aux (n-2) (fun nminus2 -> continuation(nminus1 + nminus2)))
    aux x id

type word = (char * int) list

type aExp =
    | N of int              (* Integer literal *)
    | V of string           (* Variable reference *)
    | WL                    (* Word length *)
    | PV of aExp            (* Point value lookup at word index *)
    | Add of aExp * aExp    (* Addition *)
    | Sub of aExp * aExp    (* Subtraction *)
    | Mul of aExp * aExp    (* Multiplication *)
    | CharToInt of cExp     (* NEW: Cast to integer *)

and cExp =
   | C  of char             (* Character literal *)
   | CV of aExp             (* Character lookup at word index *)
   | ToUpper of cExp        (* Convert character to upper case *)
   | ToLower of cExp        (* Convert character to lower case *)
   | IntToChar of aExp      (* NEW: Cast to character *)

let rec arithEvalSimple (exp:aExp) (word:word) (state:Map<string, int>) : int =
    match exp with
    |V s -> if (state.TryFind s).IsSome then state.[s] else 0
    |N x -> x
    |WL -> word.Length
    |PV aexp -> snd (word.[(arithEvalSimple aexp word state)]) 
    |Add(x,y) -> arithEvalSimple x word state + arithEvalSimple y word state
    |Sub(x,y) -> arithEvalSimple x word state - arithEvalSimple y word state
    |Mul(x,y) -> arithEvalSimple x word state * arithEvalSimple y word state
    |CharToInt cexp -> int (charEvalSimple cexp word state)
and
    charEvalSimple (exp:cExp) (word:word) (state:Map<string, int>) : char =
    match exp with
    |C exp -> exp
    |ToUpper exp -> System.Char.ToUpper (charEvalSimple exp word state)
    |ToLower exp -> System.Char.ToLower (charEvalSimple exp word state)
    |CV exp -> fst word.[(arithEvalSimple exp word state)]
    |IntToChar aexp -> char (arithEvalSimple aexp word state)

let rec arithEvalTail (exp:aExp) (word:word) (state:Map<string,int>) (continuation:int ->'a) :'a =
    match exp with
    |N n -> continuation n
    |V s -> continuation(if (state.TryFind s).IsSome then state.[s] else 0)
    |WL -> continuation(word.Length)
    |PV aexp ->  arithEvalTail aexp word state (fun n -> continuation (snd(word.[n])))
    |Add(x,y) -> arithEvalTail x word state (fun n -> arithEvalTail y word state (fun k -> continuation (n + k)))
    |Sub(x,y) -> arithEvalTail x word state (fun n -> arithEvalTail y word state (fun k -> continuation (n - k)))
    |Mul(x,y) -> arithEvalTail x word state (fun n -> arithEvalTail y word state (fun k -> continuation (n * k)))
    |CharToInt cexp -> charEvalTail cexp word state (fun n -> continuation (int n))
and 
    charEvalTail (exp:cExp) (word:word) (state:Map<string,int>) (continuation:char ->'a) : 'a =
    match exp with
    |C cexp -> continuation cexp
    |ToUpper cexp -> charEvalTail cexp word state (fun n -> continuation (System.Char.ToUpper n))
    |ToLower cexp -> charEvalTail cexp word state (fun n -> continuation (System.Char.ToLower n))
    |CV aexp -> arithEvalTail aexp word state (fun n -> continuation (fst word.[n]))
    |IntToChar aexp -> arithEvalTail aexp word state (fun n -> continuation (char n))

let odds = Seq.initInfinite(fun x -> x) |> Seq.filter (fun x -> x % 2 = 1)

    //This has fast lookup time for items, but recomputes it every time, and is not well suited for caching
let facts = Seq.initInfinite (fun x -> factA x) 

    //works better with caching, but sacrifices lookup time
    //it's slower than the previous version on arbitrary lookup, even for an item that's been cached, 
    //i think this is because it has to traverse the sequence
let factsAlt = 
    (1,1) |> 
    Seq.unfold (fun state -> 
        let index = snd state
        let previousvalue = fst state
        let currentvalue = previousvalue * (index)
        Some(currentvalue, (currentvalue, index + 1))
        )|>
    Seq.cache

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

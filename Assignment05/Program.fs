// Learn more about F# at http://fsharp.org

open System

let sum (m:int) (n:int) : int =
    let rec sumA (innerN:int) (acc:int) :int =
        match innerN with 
        |0 -> acc
        |innerN -> sumA (innerN-1) (innerN + m + acc)
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
    foldBackC list (fun x -> x)

let fact (n:int) :int =
    let rec factC (innerN:int) (continuation: int -> int) : int =
        match innerN with 
        |1 -> continuation 1  
        |innerN -> factC (innerN-1) (fun x -> innerN * x)
    factC n (fun x -> x)

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
        |1 -> aux 1 0 (n+1)
        |n when n = x -> nminus1 + nminus2
        |n -> aux (nminus1+nminus2) nminus1 (n+1)
    aux 0 0 1

let fibC (x:int) :int =
    let rec aux (n:int) (continuation:int -> int): int =
        match n with
        |1 -> aux (n+1) (fun x -> 1 + 0 + x)
        |n when n = x -> continuation 0
        |n -> aux (n+1) (fun x -> continuation x + x )
    aux 1 (fun x -> x)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

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



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

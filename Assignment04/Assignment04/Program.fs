// Learn more about F# at http://fsharp.org

open System
module MultiSet
    type multiset = MultiSet of (Map<'a, uint32>ref int)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

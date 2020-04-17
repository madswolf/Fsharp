﻿// Learn more about F# at http://fsharp.org

open System

open ScrabbleUtil

let readLines filePath = System.IO.File.ReadLines(filePath) |> List.ofSeq

let spawnMultiples name bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, bot)::aux (x - 1)
   
    aux >> List.rev
[<EntryPoint>]

let main argv =
    DebugPrint.debugFlag <- false // Change to false to supress debug output

    let board      = StandardBoard.standardBoard
    let alphabet   = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let dictionary = readLines "../../../EnglishDictionary.txt"
    let handSize   = 7u
    let timeout    = None
    let tiles      = English.tiles 1u
    let seed       = None
    let port       = 13001

    let players    = spawnMultiples "Oxyphenbutazone" Oxyphenbutazone.Startup.startGame 2

    // Uncomment this line to call your client
//  let players    = [("Your name here", YourClientName.Scrabble.startGame)] 

 
    do ScrabbleServer.Comm.startGame 
          board alphabet dictionary handSize timeout tiles seed port players

    0

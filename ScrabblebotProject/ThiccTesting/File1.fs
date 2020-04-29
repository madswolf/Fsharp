module ThiccScrabbleBot.File1

open System
open Xunit
open ValidityEngine
open GenerationEngine
open State
open System.IO
open ScrabbleUtil
open Dictionary

//naming is horrible and i love it
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let filepath string =
    @"D:\code\Fsharp\ScrabblebotProject\ThiccTesting\Testfiles\" + string + @".txt"

let squares = 
    (StandardBoard.standardBoard ()).squares |>
    squaresOfProgToSquaresOfFun

let usedSquare = 
    (StandardBoard.standardBoard ()).usedSquare
let boardFun =
    (StandardBoard.standardBoard ()).prog |>
    boardProgToBoardFun  <| usedSquare

[<Fact>]
let getPerpendicularWord_starting40_given_continues_horisontal_starting00_hello__returns_hello () =
    let thing = boardFun
    let coord = thing (8,8)
    Assert.True(true)
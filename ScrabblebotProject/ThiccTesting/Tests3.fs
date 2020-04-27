module ThiccScrabbleBot.Tests3
open Xunit
open Ass7.ImpParser
open System.IO


//naming is horrible and i love it
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let filepath string =
    @"D:\code\Fsharp\ScrabblebotProject\ThiccTesting\Testfiles\" + string + @".txt"

let testThingy boardName = 
    let standardboard = readLines(filepath boardName) |> Seq.fold(fun acc item -> acc + item) ""
    

    let board = (runTextParser stmParse standardboard)
    let thing = File.CreateText(filepath (boardName + "Result"))  
    thing.Write(board)
    thing.Dispose ()
    let actual = readLines(filepath (boardName + "Result")) |> Seq.fold(fun acc item -> item + acc) ""
    let expected = readLines(filepath (boardName + "Expected")) |> Seq.fold(fun acc item -> item + acc) ""
    actual = expected

[<Fact>]
let StandardBoardParseTest() =
    Assert.True(testThingy "StandardBoard")
[<Fact>]
let HoleBoardParseTest() =
    Assert.True(testThingy "HoleBoard")
[<Fact>]
let InfiniteBoardParseTest() =
    Assert.True(testThingy "InfiniteBoard")
[<Fact>]
let InfiniteHoleBoardParseTest() =
    Assert.True(testThingy "InfiniteHoleBoard")
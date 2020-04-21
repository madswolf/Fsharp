module ThiccScrabbleBot.Tests2

open System
open Xunit
open Util
open System.IO
open Dictionary

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let dict = 
    readLines "D:\code\Fsharp\ScrabblebotProject\EnglishDictionary.txt" |>
    Seq.fold (fun acc x ->  insert x acc) (empty " ")

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_hello_returns_true() =
    
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    //('E',(0,-1));('Y',(0,-2));
    let map = Map.empty
    let actual = traverseUntillLastLetterAndVerifyOrtogonalWords true map move dict true
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_hello_and_move_hello_returns_true() =
    
    let move = [((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    let things = [('H',(0,0));('E',(0,1));('Y',(0,2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = traverseUntillLastLetterAndVerifyOrtogonalWords true map move dict true
    let expected = true
    Assert.Equal(expected, actual)
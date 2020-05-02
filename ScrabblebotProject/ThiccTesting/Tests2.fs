module ThiccScrabbleBot.Tests2

open System
open Xunit
open ValidityEngine
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
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_horisontal_hello_returns_true() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((2,0),('L',0));((3,0),('L',0));((4,0),('O',0))]
    //('E',(0,-1));('Y',(0,-2));
    let map = Map.empty
    let actual = verifyCreatedWords true map move dict true 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_move_AAA_invalid_returns_False() =
    
    let move = [((0,0),('A',0));((1,0),('A',0));((2,0),('A',0))]
    let actual = verifyCreatedWords true Map.empty move dict true 1
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_AUL_and_move_horisontal_hello_returns_true() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((2,0),('L',0));((3,0),('L',0));((4,0),('O',0))]
    let things = [(('A',0),(0,1));(('U',0),(0,2));(('L',0),(0,3))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = verifyCreatedWords true map move dict true 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_AUL_and_move_vertical_hello_returns_true() =
    
    let move = [((0,0),('H',0));((0,1),('E',0));((0,2),('L',0));((0,3),('L',0));((0,4),('O',0))]
    let things = [(('A',0),(1,0));(('U',0),(2,0));(('L',0),(3,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = verifyCreatedWords true map move dict false 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_AA_ED_and_move_hello_returns_true() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((2,0),('L',0));((3,0),('L',0));((4,0),('O',0))]
    let things = [(('A',0),(0,-2));(('A',0),(0,-1));(('E',0),(0,1));(('D',0),(0,2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = verifyCreatedWords true map move dict true 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_AA_E_and_move_hello_returns_false() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((2,0),('L',0));((3,0),('L',0));((4,0),('O',0))]
    let things = [(('E',0),(0,1));(('A',0),(0,-1));(('A',0),(0,-2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = verifyCreatedWords true map move dict true 1
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_As_and_move_hello_returns_true() =
    
    let move = [((0,0),('H',0));((1,0),('E',0));((2,0),('L',0));((3,0),('L',0))]
    let things = [(('A',0),(0,-2));(('A',0),(0,-1));(('E',0),(0,1));(('D',0),(0,2)); (('A',0),(1,-2)); (('B',0),(1,-1)); (('A',0),(1,1));(('M',0),(1,2));(('O',0),(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = verifyCreatedWords true map move dict true 1
    let expected = true
    Assert.Equal(expected, actual)
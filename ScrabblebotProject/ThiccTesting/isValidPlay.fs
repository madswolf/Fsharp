module ThiccScrabbleBot.isValidPlayTests

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

let boardWithoutBoardState = 
     boardToStateBoard (ScrabbleUtil.StandardBoard.standardBoard ())

[<Fact>]
let isValidPlay_given_horisontal_hello_and_board_nothing_returns_true() =
    
    let board = boardWithoutBoardState Map.empty
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]

    let actual = isValidPlay move board dict 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isValidPlay_given_horisontal_he_SPACE_lo_and_board_nothing_returns_false() =
    
    let board = boardWithoutBoardState Map.empty
    let move = [((0,0),'H');((1,0),'E');((3,0),'L');((4,0),'O')]

    let actual = isValidPlay move board dict 1
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isValidPlay_given_horisontal_hello_skewed_and_board_nothing_returns_false() =
    
    let board = boardWithoutBoardState Map.empty
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,1),'O')]

    let actual = isValidPlay move board dict 1
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isValidPlay_given_vertical_hello_and_board_nothing_returns_true() =
    
    let board = boardWithoutBoardState Map.empty
    let move = [((0,0),'H');((0,1),'E');((0,2),'L');((0,3),'L');((0,4),'O')]

    let actual = isValidPlay move board dict 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isValidPlay_given_vertical_he_SPACE_lo_and_board_nothing_returns_false() =
    
    let board = boardWithoutBoardState Map.empty
    let move = [((0,0),'H');((0,-1),'E');((0,-3),'L');((0,-4),'O')]

    let actual = isValidPlay move board dict 1
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isValidPlay_given_horisontal_he_SPACE_lo_skewed_and_board_nothing_returns_false() =
    
    let board = boardWithoutBoardState Map.empty
    let move = [((0,0),'H');((0,-1),'E');((0,-2),'L');((0,-3),'L');((1,-4),'O')]

    let actual = isValidPlay move board dict 1
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isValidPlay_given_vertical_hello_and_board_with_aaa_returns_false() =
    
    let things = [('A',(1,2));('A',(0,2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((0,0),'H');((0,1),'E');((0,2),'L');((0,3),'L');((0,4),'O')]

    let actual = isValidPlay move board dict 1
    let expected = false
    Assert.Equal(expected, actual)



    (*
[<Fact>]
let isValidPlay_given_horisontal_hello_and_board_AAED_returns_true() =

    let things = [('A',(0,2));('A',(0,1));('E',(0,-1));('D',(0,-2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]

    let actual = isValidPlay move board dict
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_AUL_and_move_hello_returns_true() =
    
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    let things = [('A',(0,-1));('U',(0,-2));('L',(0,-3))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = traverseUntillLastLetterAndVerifyOrtogonalWords true map move dict true
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_AA_ED_and_move_hello_returns_true() =
    
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    let things = [('A',(0,2));('A',(0,1));('E',(0,-1));('D',(0,-2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = traverseUntillLastLetterAndVerifyOrtogonalWords true map move dict true
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_AA_E_and_move_hello_returns_false() =
    
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L');((4,0),'O')]
    let things = [('E',(0,1));('A',(0,-1));('A',(0,-2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = traverseUntillLastLetterAndVerifyOrtogonalWords true map move dict true
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let traverseUntillLastLetterAndVerifyOrtogonalWords_given_board_with_As_and_move_hello_returns_true() =
    
    let move = [((0,0),'H');((1,0),'E');((2,0),'L');((3,0),'L')]
    let things = [('A',(0,2));('A',(0,1));('E',(0,-1));('D',(0,-2)); ('A',(1,2)); ('B',(1,1)); ('A',(1,-1));('M',(1,-2));('O',(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let actual = traverseUntillLastLetterAndVerifyOrtogonalWords true map move dict true
    let expected = true
    Assert.Equal(expected, actual) *)
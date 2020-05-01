module ThiccScrabbleBot.IsConnectedToBoard

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
let isConnectedToBoard_given_prefix_horisontal_lo_and_board_hel_connected_returns_true() =
    
    let things = [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((3,0),('L',0));((4,0),('O',0))]

    let actual = isConnectedToOtherWords move board true 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isConnectedToBoard_given_prefix_horisontal_lo_and_board_hel_disconnected_returns_true() =
    
    let things = [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((4,0),('L',0));((5,0),('O',0))]

    let actual = isConnectedToOtherWords move board true 1
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isConnectedToBoard_given_prefix_vertical_lo_and_board_hel_connected_returns_true() =
    
    let things = [(('H',0),(0,0));(('E',0),(0,1));(('L',0),(0,2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((0,3),('L',0));((0,4),('O',0))]

    let actual = isConnectedToOtherWords move board true 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isConnectedToBoard_given_prefix_vertical_lo_and_board_hel_disconnected_returns_true() =
    
    let things = [(('H',0),(0,0));(('E',0),(0,1));(('L',0),(0,2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((0,4),('L',0));((0,5),('O',0))]

    let actual = isConnectedToOtherWords move board true 1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isConnectedToBoard_given_prefix_horisontal_hel_and_board_lo_connected_returns_true() =
    
    let things = [(('L',0),(0,3));(('L',0),(0,4))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((0,0),('H',0));((0,1),('E',0));((0,2),('L',0))]

    let actual = isConnectedToOtherWords move board true -1
    let expected = true
    Assert.Equal(expected, actual)

[<Fact>]
let isConnectedToBoard_given_suffix_vertical_hel_and_board_lo_disconnected_returns_false() =
    
    let things = [(('L',0),(0,3));(('L',0),(0,4))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((0,-1),('H',0));((0,0),('E',0));((0,1),('L',0))]

    let actual = isConnectedToOtherWords move board false -1
    let expected = false
    Assert.Equal(expected, actual)

[<Fact>]
let isConnectedToBoard_given_suffix_horisontal_hel_and_board_lo_disconnected_returns_false() =
    
    let things = [(('L',0),(3,0));(('L',0),(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things

    let board = boardWithoutBoardState map
    let move = [((-1,0),('H',0));((0,0),('E',0));((1,0),('L',0))]

    let actual = isConnectedToOtherWords move board true -1
    let expected = false
    Assert.Equal(expected, actual)







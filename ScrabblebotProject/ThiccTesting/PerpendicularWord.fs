module ThiccScrabbleBot.PerpendicularWordTests

open System
open Xunit
open ValidityEngine

[<Fact>]
let getPerpendicularWord_starting20_given_continues_horisontal_starting00_hello__returns_hello () =
    let things = [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0));(('L',0),(3,0));(('O',0),(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (2,0)
    let actual = getPerpendicularWord map startcoord false 1
    let expected = ['H';'E';'L';'L';'O'] |> List.fold (fun acc item -> acc + string item ) "" 
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let getPerpendicularWord_starting40_given_continues_horisontal_starting00_hello__returns_hello () =
    let things = [(('D',0),(0,2));(('E',0),(0,1));(('H',0),(0,0));(('A',0),(0,-2));(('A',0),(0,-1))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (0,0)
    let actual = getPerpendicularWord map startcoord true 1
    let expected = ['A';'A';'H';'E';'D';] |> List.fold (fun acc item -> acc + string item ) "" 

    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let getPerpendicularWord_given_board_with_AAHED_and_move_hello_returns_AAHED() =
    
    let things = [(('A',0),(0,-2));(('A',0),(0,-1));(('H',0),(0,0));(('E',0),(0,1));(('D',0),(0,2))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let coord = (0,0)
    let actual = getPerpendicularWord map coord true 1
    let expected = "AAHED"
    Assert.Equal(expected, actual)

[<Fact>]
let getPerpendicularWord_given_board_with_HAUL_returns_true() =
    
    let things = [(('H',0),(0,0));(('A',0),(1,0));(('U',0),(2,0));(('L',0),(3,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let coord = (0,0)
    let actual = getPerpendicularWord map coord false 1
    let expected = "HAUL"
    Assert.Equal(expected, actual)

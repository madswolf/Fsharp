module ThiccScrabbleBot.PerpendicularWordTests

open System
open Xunit
open Util

[<Fact>]
let getPerpendicularWord_starting20_given_continues_horisontal_starting00_hello__returns_hello () =
    let things = [('H',(0,0));('E',(1,0));('L',(2,0));('L',(3,0));('O',(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (2,0)
    let actual = getPerpendicularWord map startcoord false
    let expected = ['H';'E';'L';'L';'O'] |> List.fold (fun acc item -> acc + string item ) "" 
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let getPerpendicularWord_starting40_given_continues_horisontal_starting00_hello__returns_hello () =
    let things = [('D',(0,-2));('E',(0,-1));('H',(0,0));('A',(0,2));('A',(0,1))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (0,0)
    let actual = getPerpendicularWord map startcoord true
    let expected = ['A';'A';'H';'E';'D';] |> List.fold (fun acc item -> acc + string item ) "" 

    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let getPerpendicularWord_given_board_with_AAED_and_move_hello_returns_true() =
    
    let things = [('A',(0,2));('A',(0,1));('H',(0,0));('E',(0,-1));('D',(0,-2)); ('A',(1,2)); ('B',(1,1)); ('A',(1,-1));('M',(1,-2));('O',(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let coord = (0,0)
    let actual = getPerpendicularWord map coord true
    let expected = "AAHED"
    Assert.Equal(expected, actual)
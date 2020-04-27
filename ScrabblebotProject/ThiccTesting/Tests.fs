module ThiccScrabbleBot.Tests1

open System
open Xunit
open ValidityEngine

[<Fact>]
let traverseUntillNull_starting00_given_continues_horisontal_starting00_hello__returns_hello () =
    let things = [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0));(('L',0),(3,0));(('O',0),(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (0,0)
    let actual = (traverseUntillNull [] map startcoord true 1) |> List.map (fst)
    let expected = ['H';'E';'L';'L';'O']
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let traverseUntillNull_starting40_given_continues_horisontal_starting00_hello__returns_hello () =
    let things = [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0));(('L',0),(3,0));(('O',0),(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (4,0)
    let actual = traverseUntillNull [] map startcoord true -1 |> List.map (fst)
    let expected = ['H';'E';'L';'L';'O'] |> List.rev
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let traverseUntillNull_starting40_given_continues_horisontal_starting00_hello_and_oof_hello () =
    let things = [(('O',0),(-4,0));(('F',0),(-3,0));(('O',0),(-2,0));(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0));(('L',0),(3,0));(('O',0),(4,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (4,0)
    let actual = traverseUntillNull [] map startcoord true -1 |> List.map (fst)
    let expected = ['H';'E';'L';'L';'O'] |> List.rev
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let traverseUntillNull_starting00_given_continues_horisontal_starting00_hello_and_oof_returns_hello () =
    let things = [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0));(('L',0),(3,0));(('O',0),(4,0));(('O',0),(6,0));(('F',0),(7,0));(('O',0),(8,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (0,0)
    let actual = traverseUntillNull [] map startcoord true 1 |> List.map (fst)
    let expected = ['H';'E';'L';'L';'O']
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let traverseUntillNull_starting02_given_continues_horisontal_starting00_hello_and_oof_returns_llo () =
    let things = [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0));(('L',0),(3,0));(('O',0),(4,0));(('O',0),(6,0));(('F',0),(7,0));(('O',0),(8,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (2,0)
    let actual = traverseUntillNull [] map startcoord true 1 |> List.map (fst)
    let expected = ['L';'L';'O']
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let traverseUntillNull_starting00_given_continues_vertical_starting00_hello__returns_hello () =
    let things = [(('H',0),(0,0));(('E',0),(0,1));(('L',0),(0,2));(('L',0),(0,3));(('O',0),(0,4))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (0,0)
    let actual = traverseUntillNull [] map startcoord false 1 |> List.map (fst)
    let expected = ['H';'E';'L';'L';'O']
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let traverseUntillNull_starting00_given_continues_vertical_starting00_hello_and_oof_returns_hello () =
    let things = [(('H',0),(0,0));(('E',0),(0,1));(('L',0),(0,2));(('L',0),(0,3));(('O',0),(0,4));(('O',0),(0,6));(('F',0),(0,7));(('O',0),(0,8))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (0,0)
    let actual = traverseUntillNull [] map startcoord false 1 |> List.map (fst)
    let expected = ['H';'E';'L';'L';'O']
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)

[<Fact>]
let traverseUntillNull_starting04_given_continues_vertical_starting00_hello_and_oof_returns_hello () =
    let things = [(('O',0),(0,-4));(('F',0),(0,-3));(('O',0),(0,-2));(('H',0),(0,0));(('E',0),(0,1));(('L',0),(0,2));(('L',0),(0,3));(('O',0),(0,4))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let startcoord = (0,4)
    let actual = traverseUntillNull [] map startcoord false -1 |> List.map (fst)
    let expected = ['H';'E';'L';'L';'O'] |> List.rev
    Assert.Equal<Collections.Generic.IEnumerable<char>>(expected, actual)
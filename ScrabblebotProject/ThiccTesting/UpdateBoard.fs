module ThiccScrabbleBot.UpdateBoard

open System
open Xunit
open ValidityEngine
open GenerationEngine
open State
open System.IO
open ScrabbleUtil
open Dictionary

let boardWithoutBoardState = 
    boardToStateBoard (ScrabbleUtil.StandardBoard.standardBoard ())

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let tileMap : Map<uint32,tile>=
    [
    (1u,('A',1));
    (2u,('B',3));
    (3u,('C',3));
    (4u,('D',2));
    (5u,('E',1));
    (6u,('F',4));
    (7u,('G',2));
    (8u,('H',4));
    (9u,('I',1));
    (10u,('J',8));
    (11u,('K',5));
    (12u,('L',1));
    (13u,('M',3));
    (14u,('N',1));
    (15u,('O',1));
    (16u,('P',3));
    (17u,('Q',10));
    (18u,('R',1));
    (19u,('S',1));
    (20u,('T',1));
    (21u,('U',1));
    (22u,('V',4));
    (23u,('W',4));
    (24u,('X',8));
    (25u,('Y',4));
    (26u,('Z',10));
    ] |>
    List.fold (fun map x-> Map.add (fst x) (Set.add (snd x) Set.empty) map) Map.empty


[<Fact>]
let isContinuosMove_given_vertical_continuos_hello_returns_true() =
    let things = [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let board = boardWithoutBoardState map
    let move = [((3,0),(12u,('L',0)));((4,0),(15u,('O',0)));]
    let actual = (updateBoard board move).boardMap
    let expected = 
        [(('H',0),(0,0));(('E',0),(1,0));(('L',0),(2,0));(('L',0),(3,0));(('O',0),(4,0))]|> 
        List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty
    Map.map (fun index item -> Assert.Equal(item,actual.[index])) expected
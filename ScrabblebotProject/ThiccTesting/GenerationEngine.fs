module ThiccScrabbleBot.GenerationEngine

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

let dict = 
    readLines "D:\code\Fsharp\ScrabblebotProject\EnglishDictionary.txt" |>
    Seq.fold (fun acc x ->  insert x acc) (empty " ")

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

let squares = 
    (StandardBoard.standardBoard ()).squares |>
    squaresOfProgToSquaresOfFun

let boardFun =
    (StandardBoard.standardBoard ()).prog |>
    boardProgToBoardFun <| squares

let center = (StandardBoard.standardBoard ()).center

let stateWithoutBoardMapOrhand hand boardMap=
    mkBoard boardFun 0 squares center boardMap |>
    mkState tileMap 0u [] 0u dict hand 
[<Fact>]
let generateAWordFromState_given_board_with_hel_and_hand_LOL__finds_move_LO () =
    let things = [('H',(0,0));('E',(1,0));('L',(2,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let hand =
        [12u;15u] |> 
        List.fold (fun multiset item -> MultiSet.addSingle item multiset) MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand map
    let actual = generateValidMove state
    let expected = [((3,0),(12u,('L',1)));((4,0),(15u,('O',1)));]
    List.mapi (fun index item -> Assert.Equal(item,actual.[index])) expected
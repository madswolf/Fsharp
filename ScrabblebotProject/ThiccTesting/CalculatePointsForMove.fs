module ThiccScrabbleBot.CalculatePointsForMove

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

let reverse (input:string) = input |> Seq.rev |> System.String.Concat

let reverseDict =
    readLines "D:\code\Fsharp\ScrabblebotProject\EnglishDictionary.txt" |>
    Seq.map reverse |> 
    Seq.fold (fun acc x ->  insert x acc) (empty " ")

let wildcard =
    (0u ,[
    ('A',0);
    ('B',0);
    ('C',0);
    ('D',0);
    ('E',0);
    ('F',0);
    ('G',0);
    ('H',0);
    ('I',0);
    ('J',0);
    ('K',0);
    ('L',0);
    ('M',0);
    ('N',0);
    ('O',0);
    ('P',0);
    ('Q',0);
    ('R',0);
    ('S',0);
    ('T',0);
    ('U',0);
    ('A',0);
    ('V',0);
    ('W',0);
    ('X',0);
    ('Y',0);
    ('Z',0);
    ] |>
    List.fold (fun acc item -> Set.add item acc) Set.empty) 

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
    (27u,('Æ',-1))
    ] |>
    List.fold (fun map x-> Map.add (fst x) (Set.add (snd x) Set.empty) map) Map.empty |> 
    Map.add (fst wildcard) (snd wildcard)

let squares = 
    (StandardBoard.standardBoard ()).squares |>
    squaresOfProgToSquaresOfFunList

let usedSquare = 
    (StandardBoard.standardBoard ()).usedSquare
let boardFun =
    (StandardBoard.standardBoard ()).prog |>
    boardProgToBoardFun  <| usedSquare

let center = (StandardBoard.standardBoard ()).center

let stateWithoutBoardMapOrhand hand boardMap=
    mkState tileMap 0u 0u dict reverseDict hand (mkBoard boardFun 0 squares center boardMap) 5 "" []

[<Fact>]
let calculatePointsForMove_given_board_empty_and_move_vertical_HELLO__returns_74 () =
    let move = ([
        ((7,-2),(8u,('H',4)));
        ((7,-1),(5u,('E',1)));
        ((7,0),(12u,('L',1)));
        ((7,1),(12u,('L',1)));
        ((7,2),(15u,('O',1)))
    ],false)
    let hand = MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand Map.empty
    let actual = snd (calculatepointsForMove state move)
    let expected = 74
    Assert.Equal(actual,expected)

[<Fact>]
let calculatePointsForMove_given_board_empty_and_move_horisontal_HELLO__returns_74 () =
    let move = ([
        ((3,0),(8u,('H',4)));
        ((4,0),(5u,('E',1)));
        ((5,0),(12u,('L',1)));
        ((6,0),(12u,('L',1)));
        ((7,0),(15u,('O',1)))
    ],true)
    let hand = MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand Map.empty
    let actual = snd (calculatepointsForMove state move)
    let expected = 77
    Assert.Equal(actual,expected)

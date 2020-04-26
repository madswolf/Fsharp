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
    mkState tileMap 0u 0u dict hand 
[<Fact>]
let generateAWordFromState_given_board_with_hel_and_hand_LOL__finds_move_LO () =
    let things = [('H',(0,0));('E',(1,0));('L',(2,0))]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let hand =
        [12u;15u] |> 
        List.fold (fun multiset item -> MultiSet.addSingle item multiset) MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand map
    let actual = generateValidMove state
    let expected = [((2,1),(15u,('O',1)));]
    List.mapi (fun index item -> Assert.Equal(item,actual.[index])) expected

[<Fact>]
let generateAWordFromState_given_realgame1__finds_move_LO () =
    let things = [
        ('A',(-7,0));
        ('E',(-6,-1));
        ('X',(-6,0));
        ('D',(-5,-1));
        ('D',(-4,-1));
        ('E',(-4,0));
        ('O',(-3,-1));
        ('M',(-3,0));
        ('I',(-2,0));
        ('N',(-1,0));
        ('E',(0,0));
        ('N',(1,0));
        ('T',(2,0));
        ('I',(2,1));
        ('R',(2,2));
        ('E',(2,3));
        ('S',(3,3));
        ('C',(4,3));
        ('O',(5,3));
        ('T',(6,3));
        ('A',(6,4));
        ('B',(6,5));
        ]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let hand =
        [1u;5u;6u;10u;13u;18u;19u] |> 
        List.fold (fun multiset item -> MultiSet.addSingle item multiset) MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand map
    let actual = generateValidMove state
    let expected = [((6,6),(5u,('E',1)));((6,7),(18u,('R',1)));]
    List.mapi (fun index item -> Assert.Equal(item,actual.[index])) expected

[<Fact>]
let generateAWordFromState_given_realgame2__finds_move_LO () =
    let things = [
        ('A',(0,0));
        ('A',(1,0));
        ('A',(1,1));
        ('S',(1,2));
        ('E',(2,2));
        ('N',(3,2));
        ('O',(3,3));
        ('M',(3,4));
        ('B',(4,3));
        ('E',(5,3));
        ('R',(5,4));
        ('E',(5,5));
        ('I',(6,4));
        ('D',(7,4));
        ('U',(7,5));
        ('N',(7,6)) 
        ]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let hand =
        [4u;12u;15u;16u;18u;20u;22u] |> 
        List.fold (fun multiset item -> MultiSet.addSingle item multiset) MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand map
    let actual = generateValidMove state
    let expected = [((7,7),(20u,('T',1)))]
    List.mapi (fun index item -> Assert.Equal(item,actual.[index])) expected

[<Fact>]
let generateAWordFromState_given_realgame3__finds_move_LO () =
    let things = 
        [
        ('A',(0,0));
        ('G',(1,0))
        ]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let hand =
        [4u;11u;12u;14u;18u] |> 
        List.fold (fun multiset item -> MultiSet.addSingle item multiset) MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand map
    let actual = generateValidMove state
    let expected = [((0, 1), (4u, ('D', 2)))]
    List.mapi (fun index item -> Assert.Equal(item,actual.[index])) expected
    

[<Fact>]
let generateAWordFromState_given_realgame4__finds_move_LO () =
    let things = 
        [
        ('A',(0,0));
        ('D',(1,0));
        ('A',(1,1));
        ('I',(1,2));
        ('S',(1,3));
        ('E',(2,3));
        ('l',(3,3));
        ('A',(3,4));
        ('R',(3,5));
        ('O',(4,5));
        ('T',(5,5));
        ('A',(5,6));
        ('E',(5,7));
        ('D',(6,7));
        ]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let hand =
        [14u;15u;18u;19u;21u;22u;24u] |> 
        List.fold (fun multiset item -> MultiSet.addSingle item multiset) MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand map
    let actual = generateValidMove state
    let expected = [((7, 7), (19u, ('S', 1)))]
    List.mapi (fun index item -> Assert.Equal(item,actual.[index])) expected

[<Fact>]
let generateAWordFromState_given_realgame5__finds_move_LO () =
    let things = 
        [
        ('D',(0,0));
        ('E',(1,0));
        ('E',(1,1));
        ('N',(2,1));
        ('L',(1,2));
        ('Y',(1,3));
        ('E',(2,2));
        ('D',(3,2));
        ('A',(3,3));
        ('D',(3,4));
        ('W',(4,3));
        ('A',(5,3));
        ('E',(4,4));
        ('E',(5,4));
        ('R',(6,4));
        ('F',(4,5));
        ('T',(4,6));
        ('I',(5,6));
        ('E',(6,6));
        ('M',(6,7));
        ('O',(7,7));
        ]
    let map = List.fold(fun acc item -> Map.add (snd item) (fst item) acc) Map.empty things
    let hand =
        [5u;7u;9u;12u;14u;22u] |> 
        List.fold (fun multiset item -> MultiSet.addSingle item multiset) MultiSet.empty
    let state = stateWithoutBoardMapOrhand hand map
    let actual = generateValidMove state
    let expected = [((7, 7), (19u, ('S', 1)))]
    List.mapi (fun index item -> Assert.Equal(item,actual.[index])) expected

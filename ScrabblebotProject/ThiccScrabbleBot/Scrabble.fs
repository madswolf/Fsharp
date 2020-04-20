namespace ThiccScrabbleBot

open ScrabbleLib
open StateMonad
open ScrabbleServer
open JParsec.TextParser
open JParsec.Parser
open Ass7.ImpParser
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open JParsec
open Eval

open System.Net.Sockets
open System.IO
open DebugPrint

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, and your player number but it could, potentially, 
    // keep track of other useful
    // information, such as number of players, player turn, etc.

    type board = {
        boardFun      : boardFun
        boardMap      : Map<coord,char>
        usedSquare    : int
        squares       : Map<int,Map<int,squareFun>>
        center        : coord
    }

    let mkBoard bfun us sq center map = { boardFun = bfun; boardMap = map; usedSquare = us; squares = sq; center = center}

    let updateBoard board ms =
            List.fold (fun acc item -> Map.add (fst item) (fst (snd (snd item))) acc) board.boardMap ms |> 
            mkBoard board.boardFun board.usedSquare board.squares board.center

    type state = {
        playerNumber  : uint32
        players       : uint32 list
        previousPlayer: uint32
        hand          : MultiSet.MultiSet<uint32>
        board         : board
    }

    let mkState pn players pp h board = { playerNumber = pn; players = players; previousPlayer = pp; hand = h; board = board}

    let newState pn hand = mkState pn hand

    let updateHand hand ms newPieces =
        List.map (fun x -> snd x  |> fst) ms |>
        List.fold (fun acc item -> MultiSet.removeSingle item acc) hand |>
        MultiSet.sum (List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty newPieces) 

    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading

    let isInDictionary word =
        true
    
    //A function that traverses the given map horisontally or vertically positively or negatively depending on arguments
    let rec traverseUntillNull acc (map:Map<coord,char>) coord horisontal up=
     let result = map.TryFind coord
     if (result).IsSome 
     then 
         let coord = if horisontal then (((fst coord) + up), snd coord) else (fst coord , (snd coord) + up)
         traverseUntillNull (result.Value :: acc) map coord horisontal up
     else acc
    
    let rec traverseUntillLastLetterAndVerifyOrtogonalWords acc (map:Map<coord,char>) (move:(coord * char) list) (horisontal:bool) :bool =
     if (not acc)
     then acc
     else 
         match move with
         |x::xs -> 
             let thing = traverseUntillNull [] map (fst x) (not horisontal) 1
             let acc = isInDictionary (traverseUntillNull thing map (fst x) (not horisontal) -1)
             traverseUntillLastLetterAndVerifyOrtogonalWords acc map xs horisontal
         |_ -> acc
         
    
    let isValidPlay (move:(coord * (uint32 * (char * int))) list) (board:State.board) : bool =
    
     //check if coords are not in holes
     let boardFun = board.boardFun
     let includesHoles = 
         List.fold (
             fun acc x -> 
                 if acc 
                 then acc 
                 else 
                     (boardFun (fst x)) |>
                     fun thing -> 
                         if thing.IsSome 
                         then thing.Value.IsEmpty 
                         else false
         ) false move
     if includesHoles 
     then false
     //check for changed words by move
     //check hori or verti if hori then only check horisontally on the last tile and vice versa
     //for each tile check the ortogonal directions of the placement direction and verify that it creates valid words
    
     else true
    
    
    let boardProgToBoardFun boardProg squares  : boardFun= 
      runTextParser stmParse boardProg |> 
      (
         function
         |Success (x,_) -> x 
         |_ -> failwith "oopps"
      ) |>
      stmntToBoardFun
      <| squares
    
    let squaresOfProgToSquaresOfFun squares =
     Map.map (
         fun id map ->
             Map.map (
                 fun key value -> 
                     runTextParser stmParse value |>
                     (
                         function
                         |Success (x,_) -> x 
                         |_ -> failwith "oopps"
                     ) |>
                     stmntToSquareFun
             ) map
     ) squares
    
    let playGame cstream pieces (st : State.state) =
        
        let passedOrEquvalent (st:State.state) pid = State.mkState st.playerNumber st.players pid st.hand st.board
            
        let rec aux (st : State.state) =
            Thread.Sleep(5000) // only here to not confuse the pretty-printer. Remove later.
            Print.printHand pieces (State.hand st)

            let players = st.players
            let pp = st.previousPlayer
            let pn = st.playerNumber

            let isPlayerTurn = 
                snd (List.fold (fun acc item -> if fst acc = pp && item = pn then (fst acc, true) else (item,false)) (0u,false) players)

            if isPlayerTurn
            then
                //calculate move and send 
                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', (check if updated) note the absence of state between the last inputs)\n\n"
                let input =  System.Console.ReadLine()
                let move = RegEx.parseMove input
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)

            //wait for next message from server and update state depending on response
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) null) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                //remove the played pieces from the hand and add new pieces to it
                //update internal board state with move

                let hand = State.updateHand st.hand ms newPieces

                let board = State.updateBoard st.board ms

                let st' = State.mkState st.playerNumber st.players st.previousPlayer hand board
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                //update internal board and change previous player

                let board = State.updateBoard st.board ms
                let st' = State.mkState st.playerNumber st.players pid st.hand board
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                //update previous player
                aux (passedOrEquvalent st pid)
            |RCM (CMPassed pid) -> 
                //update previous player
                aux (passedOrEquvalent st pid)
            |RCM (CMChange (pid, numTiles)) -> 
                //update previous player
                aux (passedOrEquvalent st pid)
            |RCM (CMTimeout pid) ->
                //update previous player
                aux (passedOrEquvalent st pid)
            | RCM (CMGameOver _) -> () // end the misery
            | RCM (CMForfeit pid) -> 
                //remove player from list of players
                let players = List.filter (fun x -> x = pid) st.players
                let st' = State.mkState st.playerNumber players st.previousPlayer st.hand st.board
                aux st'
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

         
    

    let startGame 
            (boardP : boardProg)
            (alphabet : string)
            (words : string list)
            (numPlayers : uint32)
            (playerNumber : uint32)
            (playerTurn  : uint32)
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option)
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)


        
        let players = [1u..numPlayers]
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        let squares = squaresOfProgToSquaresOfFun boardP.squares
        let boardFun = boardProgToBoardFun boardP.prog Map.empty
        let board = State.mkBoard boardFun boardP.usedSquare squares boardP.center Map.empty

        fun () -> playGame cstream tiles (State.mkState playerNumber players playerTurn handSet board)
    

        
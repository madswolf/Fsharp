namespace ThiccScrabbleBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open ValidityEngine
open GenerationEngine
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

module Scrabble =
    open System.Threading
    open State
    
    let playGame cstream (st:state) =
        
        let passedOrEquvalent (st:state) pid = mkState st.tiles st.playerNumber st.players pid st.dictionary st.hand st.board
            
        let rec aux (st:state) =
            Thread.Sleep(5000) // only here to not confuse the pretty-printer. Remove later.
            Print.printHand st.tiles (hand st)

            let players = st.players
            let pp = st.previousPlayer
            let pn = st.playerNumber

            let isPlayerTurn = 
                snd (List.fold (fun acc item -> if fst acc = pp && item = pn then (fst acc, true) else (item,false)) (0u,false) players)

            if isPlayerTurn
            then
                let move = generateValidMove st
                printfn "%A" (moveToString move)
                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', (check if updated) note the absence of state between the last inputs)\n\n"
                let input =  System.Console.ReadLine()
                let move = RegEx.parseMove input
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)

            //wait for next message from server and update state depending on response
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (playerNumber st) null) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                //remove the played pieces from the hand and add new pieces to it
                //update internal board state with move

                let hand = updateHand st.hand ms newPieces

                let board = updateBoard st.board ms

                let st' = mkState st.tiles st.playerNumber st.players st.previousPlayer st.dictionary hand board
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                //update internal board and change previous player

                let board = updateBoard st.board ms
                let st' = mkState st.tiles st.playerNumber st.players pid st.dictionary st.hand board
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
                let st' = mkState st.tiles st.playerNumber players st.previousPlayer st.dictionary st.hand st.board
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

        let dict = Dictionary.mkDict words
        let board = boardToStateBoardWithMap boardP
        
        fun () -> playGame cstream (mkState tiles playerNumber players playerTurn dict handSet board)
    

        
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
        
        let passedOrEquvalent (st:state) = mkState st.tiles (st.movesUntillTurn - 1u) st.numberOfPlayers st.dictionary st.reverseDictionary st.hand st.board
            
        let rec aux (st:state) =
            //Thread.Sleep(5000) // only here to not confuse the pretty-printer. Remove later.
            Print.printHand st.tiles (hand st)

            let state = st

            if st.movesUntillTurn = 0u
            then
                let move = generateValidMove st
                let input =  System.Console.ReadLine()
                let moveString = moveToString move
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (playerNumber st) (moveString)) // keep the debug lines. They are useful.
                if move = [] 
                then 
                    printfn "changed hand"
                    send cstream (SMChange (MultiSet.toList st.hand))
                else 
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

                let st' = mkState st.tiles (st.numberOfPlayers - 1u ) st.numberOfPlayers st.dictionary st.reverseDictionary hand board
                aux st'
            | RCM(CMChangeSuccess (newPieces)) ->
                let hand = updateHand MultiSet.empty [] newPieces
                let st' = mkState st.tiles st.movesUntillTurn st.numberOfPlayers st.dictionary st.reverseDictionary hand st.board
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                //update internal board and change previous player

                let board = updateBoard st.board ms
                let st' = mkState st.tiles (st.movesUntillTurn - 1u) st.numberOfPlayers st.dictionary st.reverseDictionary st.hand board
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                //update previous player
                aux (passedOrEquvalent st)
            |RCM (CMPassed pid) -> 
                //update previous player
                aux (passedOrEquvalent st)
            |RCM (CMChange (pid, numTiles)) -> 
                //update previous player
                aux (passedOrEquvalent st)
            |RCM (CMTimeout pid) ->
                //update previous player
                aux (passedOrEquvalent st)
            | RCM (CMGameOver _) -> () // end the misery
            | RCM (CMForfeit pid) -> 
                //remove player from list of players
                let st' = mkState st.tiles (st.movesUntillTurn - 1u) (st.numberOfPlayers-1u) st.dictionary st.reverseDictionary st.hand st.board
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
        
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        let dict = Dictionary.mkDict words
        let reverse (input:string) = input |> Seq.rev |> System.String.Concat
        let revDict = List.map (reverse) words |> Dictionary.mkDict
        let board = boardToStateBoardWithMap boardP
        let movesUntilYourTurn = ((numPlayers - ( playerTurn -  playerNumber)) % numPlayers) 
        fun () -> playGame cstream (mkState tiles movesUntilYourTurn numPlayers  dict revDict handSet board)
    

        
namespace ThiccScrabbleBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open ValidityEngine
open GenerationEngine
open System.IO
open DebugPrint

 module Print =
    
    let boardMapToString map =
        Map.fold (fun acc coord tile -> acc + sprintf "(%A, %A); \n" tile coord ) "" map
    
    let handToString pieces hand =
           hand |>
           MultiSet.fold (fun acc x i -> acc + sprintf " %d -> (%A, %d)\n" x (Map.find x pieces) i) ""

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module Scrabble =
    open System.Threading
    open State
    
    let filepath string =
        @"D:\code\Fsharp\ScrabblebotProject\ThiccTesting\Testfiles\" + string + @".txt"


    let playGame cstream (st:state)=
        
        //tick towards 0
        let passedOrEquvalent (st:state) = mkState st.tiles (st.movesUntillTurn - 1u) st.numberOfPlayers st.dictionary st.reverseDictionary st.hand st.board st.handSize st.errors  st.performance
            
        let rec aux (st:state) =

            let st =
                if st.movesUntillTurn = 0u
                then
                    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                    let move = generateMostCashMoneyMoveParallel st
                    stopWatch.Stop()
                    let st' = mkState st.tiles (st.numberOfPlayers - 1u ) st.numberOfPlayers st.dictionary st.reverseDictionary st.hand st.board st.handSize st.errors (stopWatch.Elapsed::st.performance) 
                    let moveString = moveToString move
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (playerNumber st) (moveString)) // keep the debug lines. They are useful.
                    if move = [] 
                    then 
                        send cstream (SMChange (MultiSet.toList st.hand))
                    else 
                        send cstream (SMPlay move)
                    st'
                else 
                    st

            //wait for next message from server and update state depending on response
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (playerNumber st) null) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                //remove the played pieces from the hand and add new pieces to it
                //update internal board state with move

                let hand = updateHand st.hand ms newPieces 

                let board = updateBoard st.board ms

                let st' = mkState st.tiles (st.numberOfPlayers - 1u ) st.numberOfPlayers st.dictionary st.reverseDictionary hand board st.handSize st.errors st.performance
                aux st'
            | RCM(CMChangeSuccess (newPieces)) ->
                //update hand
                let hand = updateHand MultiSet.empty [] newPieces
                let st' = mkState st.tiles st.movesUntillTurn st.numberOfPlayers st.dictionary st.reverseDictionary hand st.board st.handSize st.errors st.performance
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                //update internal board
                let board = updateBoard st.board ms
                let st' = mkState st.tiles (st.movesUntillTurn - 1u) st.numberOfPlayers st.dictionary st.reverseDictionary st.hand board st.handSize st.errors st.performance
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                debugPrint (sprintf "\n failing move: %A" ms)
                aux (mkState st.tiles (st.movesUntillTurn - 1u) st.numberOfPlayers st.dictionary st.reverseDictionary st.hand st.board st.handSize (sprintf "%A \n Failing move: %A \n Hand: \n Board: %A \n%A" st.errors ms (Print.handToString st.tiles (hand st)) (Print.boardMapToString st.board.boardMap)) st.performance)
            |RCM (CMPassed pid) -> 
                aux (passedOrEquvalent st)
            |RCM (CMChange (pid, numTiles)) -> 
                aux (passedOrEquvalent st)
            |RCM (CMTimeout pid) ->
                aux (passedOrEquvalent st)
            | RCM (CMGameOver _) -> 
                //File.WriteAllText((filepath "errors"),st.errors + sprintf "%A average time for move" ((st.performance |> List.map (fun x -> x.Milliseconds) |> List.sum)/st.performance.Length))
                () // end the misery
            | RCM (CMForfeit pid) -> 
                //reduce number of players
                let st' = mkState st.tiles (st.movesUntillTurn - 1u) (st.numberOfPlayers-1u) st.dictionary st.reverseDictionary st.hand st.board st.handSize st.errors  st.performance
                aux st'
            | RGPE err -> aux (mkState st.tiles st.movesUntillTurn st.numberOfPlayers st.dictionary st.reverseDictionary st.hand st.board st.handSize (sprintf "%A \n Gameplay Error: %A \n Hand: \n Board: %A \n%A" st.errors err (Print.handToString st.tiles (hand st)) (Print.boardMapToString st.board.boardMap)) st.performance)
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
        fun () -> playGame cstream (mkState tiles movesUntilYourTurn numPlayers  dict revDict handSet board (int (MultiSet.size handSet)) "" [])
    

        
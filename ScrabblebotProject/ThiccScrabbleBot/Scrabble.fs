namespace ThiccScrabbleBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open GenerationEngine
open System.IO
open DebugPrint

module Scrabble =
    open State
    
    let filepath string =
        @"D:\code\Fsharp\ScrabblebotProject\ThiccTesting\Testfiles\" + string + @".txt"

    let playGame cstream (st:state)=
        let passedOrEquvalent (st:state) = mkState st.tiles (st.movesUntillTurn - 1u) st.numberOfPlayers st.dictionary st.reverseDictionary st.hand st.board st.handSize st.errors  st.performance
        let rec aux (st:state) =
            let st =
                if st.movesUntillTurn = 0u
                then
                    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                    let move = generateMostCashMoneyMoveParallel st
                    stopWatch.Stop()
                    let st' = {st with movesUntillTurn = (st.numberOfPlayers - 1u); performance = (stopWatch.Elapsed::st.performance)}
                    if move.IsEmpty
                    then send cstream (SMChange (MultiSet.toList st.hand))
                    else send cstream (SMPlay move)
                    st'
                else 
                    st

            //wait for next message from server and update state depending on response
            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                //remove the played pieces from the hand and add new pieces to it
                //update internal board state with move
                aux {st with 
                        movesUntillTurn = (st.numberOfPlayers - 1u); 
                        board = (updateBoard st.board ms); 
                        hand = (updateHand st.hand ms newPieces)
                    }
            | RCM(CMChangeSuccess (newPieces)) ->
                aux {st with hand = (updateHand MultiSet.empty [] newPieces)}
            | RCM (CMPlayed (pid, ms, points)) ->
                aux {st with movesUntillTurn = (st.movesUntillTurn - 1u) ; board = (updateBoard st.board ms)}
            | RCM (CMPlayFailed (pid, ms)) ->
                debugPrint (sprintf "\n failing move: %A" ms)
                let errors = 
                    (sprintf "%A \n Failing move: %A \n Hand: \n Board: %A \n%A" 
                    st.errors 
                    ms 
                    (handToString st.tiles (st.hand)) 
                    (boardMapToString st.board.boardMap))
                aux {st with movesUntillTurn = (st.movesUntillTurn - 1u); errors = errors}
            |RCM (CMPassed pid) -> 
                aux (passedOrEquvalent st)
            |RCM (CMChange (pid, numTiles)) -> 
                aux (passedOrEquvalent st)
            |RCM (CMTimeout pid) ->
                aux (passedOrEquvalent st)
            | RCM (CMGameOver _) -> 
                //write logfile
                File.WriteAllText((filepath "errors"),st.errors + sprintf "%A average time for move" ((st.performance |> List.map (fun x -> x.Milliseconds) |> List.sum)/st.performance.Length))
                () // end the misery
            | RCM (CMForfeit pid) -> 
                //reduce number of players
                aux {st with movesUntillTurn = (st.movesUntillTurn - 1u); numberOfPlayers = (st.numberOfPlayers - 1u)}
            | RGPE err -> aux ({st with errors = (sprintf "%A \n Gameplay Error: %A \n Hand: \n Board: %A \n%A" st.errors err (handToString st.tiles (st.hand)) (boardMapToString st.board.boardMap))})
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
    

        
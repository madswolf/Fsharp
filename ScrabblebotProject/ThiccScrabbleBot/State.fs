module State 
    open Eval
    open Dictionary
    open ScrabbleUtil
    open Ass7.ImpParser
    
    type move = (coord * (uint32 * (char * int))) list
    type board = {
        boardFun      : boardFun
        boardMap      : Map<coord,(char * int)>
        usedSquare    : int
        squares       : Map<int,square>
        center        : coord
    }

    let mkBoard bfun us sq center map = { boardFun = bfun; boardMap = map; usedSquare = us; squares = sq; center = center}

    let updateBoard board (ms:move) =
            List.fold (fun acc item -> Map.add (fst item)  (snd (snd item)) acc) board.boardMap ms |> 
            mkBoard board.boardFun board.usedSquare board.squares board.center

    type state = {
        movesUntillTurn  : uint32
        numberOfPlayers : uint32
        dictionary    : Dictionary
        reverseDictionary : Dictionary
        hand          : MultiSet.MultiSet<uint32>
        board         : board
        tiles         : Map<uint32, tile>
        handSize      : int
        errors        : string
        performance : System.TimeSpan list
    }

    let mkState tiles moves playerNum dict revDict h board handsize errors performance= {tiles = tiles; movesUntillTurn = moves; numberOfPlayers = playerNum; dictionary = dict; reverseDictionary = revDict; hand = h; board = board; handSize = handsize; errors = errors; performance = performance}
    
    let updateHand hand (ms:move) newPieces =
        List.map (fun x -> snd x  |> fst) ms |>
        List.fold (fun acc item -> MultiSet.removeSingle item acc) hand |>
        MultiSet.sum (List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty newPieces) 

    let boardMapToString map =
        Map.fold (fun acc coord tile -> acc + sprintf "(%A, %A); \n" tile coord ) "" map
    
    let handToString (pieces:Map<uint32,tile>) hand =
           hand |>
           MultiSet.fold (fun acc x i -> acc + sprintf " %d -> (%A, %d)\n" x (Map.find x pieces) i) ""

    let boardProgToBoardFun boardProg usedSquare  : boardFun= 
        let stm = runTextParser stmParse boardProg  
        stmntToBoardFun stm usedSquare 
    
    let squaresOfProgToSquaresOfFun squares =
     Map.map (
         fun id map ->
             Map.map (
                 fun key value -> 
                     runTextParser stmParse value |>
                     stmntToSquareFun
             ) map
     ) squares

    let squaresOfProgToSquaresOfFunList (squares:Map<int,squareProg>) : Map<int,square> =
        Map.map (
            fun id map ->
                Map.fold (
                    fun acc key value -> 
                        ((key), (runTextParser stmParse value |> stmntToSquareFun)) :: acc
                ) [] map
        ) squares

    let boardToStateBoard (boardP:ScrabbleUtil.boardProg) map = 
        let squaresList = boardP.squares |> squaresOfProgToSquaresOfFunList 
        let boardFun = boardProgToBoardFun boardP.prog boardP.usedSquare
        mkBoard boardFun boardP.usedSquare squaresList boardP.center map

    let boardToStateBoardWithMap (boardP:ScrabbleUtil.boardProg)= 
        boardToStateBoard boardP Map.empty
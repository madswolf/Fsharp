module State 
    open Eval
    open Dictionary
    
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
        dictionary          : Dictionary
        hand          : MultiSet.MultiSet<uint32>
        board         : board
    }

    let mkState pn players pp dict h board = { playerNumber = pn; players = players; previousPlayer = pp; dictionary = dict; hand = h; board = board}

    let newState pn hand = mkState pn hand

    let updateHand hand ms newPieces =
        List.map (fun x -> snd x  |> fst) ms |>
        List.fold (fun acc item -> MultiSet.removeSingle item acc) hand |>
        MultiSet.sum (List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty newPieces) 

    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
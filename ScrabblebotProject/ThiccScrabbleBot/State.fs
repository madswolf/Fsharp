module State 
    open Eval
    open Dictionary
    open ScrabbleUtil
    
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, and your player number but it could, potentially, 
    // keep track of other useful
    // information, such as number of players, player turn, etc.
    type move = (coord * (uint32 * (char * int))) list

    let moveToString (move:move) : string=
        let slotToString (move:coord * (uint32 * (char * int))) =
            let coord = fst move
            let x = fst coord
            let y = snd coord
            let tile = snd move
            let tileID = fst tile
            let tileValue = snd tile
            let char = fst tileValue
            let points = snd tileValue
            "("+ string x + " " + string y + " " + string tileID + " " + string char + " " + string points+")"
        List.fold(fun acc thing -> acc + " " + slotToString thing) "" move


    type board = {
        boardFun      : boardFun
        boardMap      : Map<coord,(char * int)>
        usedSquare    : int
        squares       : Map<int,Map<int,squareFun>>
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
        errors        : string
    }

    let mkState tiles moves playerNum dict revDict h board errors= {tiles = tiles; movesUntillTurn = moves; numberOfPlayers = playerNum; dictionary = dict; reverseDictionary = revDict; hand = h; board = board; errors = errors}

    let newState pn hand = mkState pn hand

    let updateHand hand (ms:move) newPieces =
        List.map (fun x -> snd x  |> fst) ms |>
        List.fold (fun acc item -> MultiSet.removeSingle item acc) hand |>
        MultiSet.sum (List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty newPieces) 

    let playerNumber st  = st.movesUntillTurn
    let hand st          = st.hand
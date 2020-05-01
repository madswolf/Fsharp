module ValidityEngine
    open Ass7.ImpParser
    open Dictionary
    open State
    open Eval
    open ScrabbleUtil

    let isMoveOverCenter center move =
        List.fold (fun acc item -> if acc then acc else fst item = center) false move

    let changeCoordAccordingToHorisontalAndUp coord horisontal up =
        if horisontal then (((fst coord) + up), snd coord) else (fst coord , (snd coord) + up)
    
    let addMoveToMap move map =
        Map.add (fst move) (snd move) map

    let rec traverseUntillNullAccumulateMove acc (map:Map<coord,char * int>) coord horisontal up  : (coord * (char * int )) list=
        let result = map.TryFind coord
        if (result).IsSome 
        then 
            let index = (coord,result.Value)
            let coord = changeCoordAccordingToHorisontalAndUp coord horisontal up
            traverseUntillNullAccumulateMove (index :: acc) map coord horisontal up
        else (List.rev acc)

    let getPerpendicularMove  map x horisontal up : ((coord * (char * int)) list) =
        //get letters to the right/below of center
        let coord = changeCoordAccordingToHorisontalAndUp  x (not horisontal) (1 * up)
        let right = traverseUntillNullAccumulateMove ([]) map coord (not horisontal) (1 * up)
        //get letters from center to left/above 
        let left = (traverseUntillNullAccumulateMove ([]) map  x (not horisontal) (-1 * up))
        let resultingword = (left |> List.rev) @ (right) 
        (resultingword) 

    let rec traverseUntillLastLetterAndAccumulateOrtogonalMoves acc (map:Map<coord,char * int>) (move:(coord * (char * int)) list) (horisontal:bool) up : ((coord * (char * int)) list) list =
        match move with
        |x::y::xs -> 
            let x = x
            let map = (addMoveToMap x map)
            let resultingWord = getPerpendicularMove map (fst x) horisontal up
            let acc = resultingWord :: acc
            traverseUntillLastLetterAndAccumulateOrtogonalMoves acc map (y::xs)horisontal up
        |x::xs -> 
            let map = (addMoveToMap x map)
            let perpendicularMove = getPerpendicularMove map (fst x) horisontal up
            let resultingWord = getPerpendicularMove map (fst x) (not horisontal) up

            resultingWord :: perpendicularMove:: acc
        |_ -> acc


    //A function that traverses the given map horisontally or vertically positively or negatively depending on arguments
    let rec traverseUntillNull acc (map:Map<coord,char * int>) coord horisontal up =
        let result = map.TryFind coord
        if (result).IsSome 
        then 
            let coord = changeCoordAccordingToHorisontalAndUp coord horisontal up
            traverseUntillNull (result.Value :: acc) map coord horisontal up
        else List.rev acc

    let getPerpendicularWord map x horisontal up : string =
        //get letters to the right/below of center
        let coord = changeCoordAccordingToHorisontalAndUp  x (not horisontal) (1 * up)
        let right = traverseUntillNull [] map coord (not horisontal) (1 * up)
        //get letters from center to left/above 
        let left = (traverseUntillNull [] map  x (not horisontal) (-1 * up)) |> List.rev
        let resultingword = left @ right 
        (resultingword) |> List.fold (fun acc item -> acc + string (fst item) ) "" 
    
    //does not work right now ignores the one in the middle
    let rec traverseUntillLastLetterAndVerifyOrtogonalWords acc (map:Map<coord,char * int>) (move:(coord * (char * int)) list) (dict:Dictionary) (horisontal:bool) up:bool =
        if (not acc)
        then acc
        else 
            match move with
            |x::y::xs -> 
                let map = (addMoveToMap x map)
                let resultingWord = getPerpendicularWord map (fst x) horisontal up
                let acc = isWord (resultingWord) dict
                traverseUntillLastLetterAndVerifyOrtogonalWords acc map (y::xs) dict horisontal up
            |x::xs -> 
                let map = (addMoveToMap x map)
                let perpendicularWord = getPerpendicularWord map (fst x) horisontal up
                let resultingWord = getPerpendicularWord map (fst x) (not horisontal) up

                (isWord resultingWord dict) && (isWord perpendicularWord dict)
            |_ -> acc

    let dict list = 
            list|>
            Seq.fold (fun acc x ->  insert x acc) (empty " ")
        
    let getXorYByHorisontal horisontal coord=
        if horisontal then fst coord else snd coord

    let isContinuosMove (move:(coord * (char * int)) list) horisontal =
        let getXorY = getXorYByHorisontal horisontal
        let getNotXorY = getXorYByHorisontal (not horisontal)
        let move = move |> List.sortBy (fun x -> getXorY(fst x))

        let isContinous = fun acc coord -> 
            if fst acc
            then 
                let isDifferenceOne = (abs (getXorY(snd acc) - getXorY(fst coord)) = 1)
                let isDifferenceZero = (abs (getNotXorY(snd acc) - getNotXorY(fst coord)) = 0)
                let isContinues = isDifferenceOne && isDifferenceZero
                (isContinues, fst coord)
            else acc

        List.fold (isContinous
        ) (true,(fst move.[1])) move |>
        fst
    let convertToValidationMove move : (coord * (char * int)) list=
         List.map (fun x -> (fst x , snd(snd x))) move

    let isConnectedToOtherWords (originalMove:(coord * (char * int)) list) (board:board) horisontal up : bool =
        let originalMap = board.boardMap
        let rec aux (acc:bool) map (move:(coord * (char * int)) list) =
            match move with 
            |x::y::xs ->
                let x = x
                let map = (addMoveToMap x map)
                let perpendicularWord = (getPerpendicularWord map (fst x) horisontal up)
                if perpendicularWord.Length <> 1
                then true
                else
                    aux acc map (y::xs)
            |x::xs ->
                let map = (addMoveToMap x map)
                let resultingWord = (getPerpendicularWord map (fst x) (not horisontal) up)
                if resultingWord.Length <> originalMove.Length
                then true 
                else acc
            |_ -> acc
        aux false originalMap originalMove 

    let isValidPlay (move:(coord * (char * int)) list) (board:board) dict horisontal up: bool =
        //check if coords are not in holes
        let map = board.boardMap
        let boardFun = board.boardFun
        let isStartingMove = map.Count = 0
        let isMoveOverCenter = (if isStartingMove then (isMoveOverCenter board.center move) else true)
        let includesHoles = 
            List.fold (
                fun acc x -> 
                    if acc 
                    then acc 
                    else 
                        (boardFun (fst x) board.boardMap) |>
                        fun thing -> 
                            if thing <> -1
                            then false 
                            else true
            ) false move

        let overlapsWithExistingTiles = 
            List.fold (
                fun acc x -> 
                    if acc 
                    then acc 
                    else 
                        (Map.tryFind (fst x) map) |>
                        fun thing -> 
                            if thing.IsSome 
                            then true 
                            else false
            ) false move

        //establish direction
        if move.Length = 1
        then 
            if (not overlapsWithExistingTiles) && (not includesHoles)
            then
                traverseUntillLastLetterAndVerifyOrtogonalWords true board.boardMap move dict true up
            else false
        else
            //check if continuos
            let isContinous = isContinuosMove move horisontal
            if (not includesHoles) && (not overlapsWithExistingTiles) && isContinous
            then 
                if isStartingMove 
                then 
                    if isMoveOverCenter
                    then traverseUntillLastLetterAndVerifyOrtogonalWords true board.boardMap move dict horisontal up
                    else false
                else 
                    //check for changed words by move
                    //if hori then only check horisontally on the last tile and vice versa
                    //for each tile check the ortogonal directions of the placement direction and verify that it creates valid words
                    traverseUntillLastLetterAndVerifyOrtogonalWords true board.boardMap move dict horisontal up
            else 
                false
    
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
        let squares = boardP.squares |> squaresOfProgToSquaresOfFun
        let squaresList = boardP.squares |> squaresOfProgToSquaresOfFunList 
        let boardFun = boardProgToBoardFun boardP.prog boardP.usedSquare
        mkBoard boardFun boardP.usedSquare squaresList boardP.center map

    let boardToStateBoardWithMap (boardP:ScrabbleUtil.boardProg)= 
        boardToStateBoard boardP Map.empty

    //let convertFromServerMoveToVerifyMove (move:move) : (coord * char) list=

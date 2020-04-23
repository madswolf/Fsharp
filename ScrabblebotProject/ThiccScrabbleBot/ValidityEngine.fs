module ValidityEngine
    open Ass7.ImpParser
    open Dictionary
    open State
    open Eval
    open ScrabbleUtil

    let changeCoordAccordingToHorisontalAndUp coord horisontal up =
        if horisontal then (((fst coord) + up), snd coord) else (fst coord , (snd coord) + up)
    
    let addMoveToMap move map =
        Map.add (fst move) (snd move) map

    //A function that traverses the given map horisontally or vertically positively or negatively depending on arguments
    let rec traverseUntillNull acc (map:Map<coord,char>) coord horisontal up =
        let result = map.TryFind coord
        if (result).IsSome 
        then 
            let coord = changeCoordAccordingToHorisontalAndUp coord horisontal up
            traverseUntillNull (result.Value :: acc) map coord horisontal up
        else List.rev acc

    let getPerpendicularWord map x horisontal =
        
        if horisontal
        then 
            //perpendicular word is vertical

            //get letters above center
            let coord = changeCoordAccordingToHorisontalAndUp  x (not horisontal) 1
            let above = traverseUntillNull [] map coord (not horisontal) 1 |> List.rev

            //get letter from center and below
            let below = traverseUntillNull [] map x (not horisontal) -1

            let resultingword = above @ below
            (resultingword) |> List.fold (fun acc item -> acc + string item ) "" 
        else 
            //perpendicular word is horisontal

            //get letters to the right og center
            let coord = changeCoordAccordingToHorisontalAndUp  x (not horisontal) 1
            let right = traverseUntillNull [] map coord (not horisontal) 1 
            //get letters from center to left 
            let left = (traverseUntillNull [] map  x (not horisontal) -1) |> List.rev
            let resultingword = left @ right 
            (resultingword) |> List.fold (fun acc item -> acc + string item ) "" 
         

    
    //does not work right now ignores the one in the middle
    let rec traverseUntillLastLetterAndVerifyOrtogonalWords acc (map:Map<coord,char>) (move:(coord * char) list) (dict:Dictionary) (horisontal:bool) :bool =
        if (not acc)
        then acc
        else 
            match move with
            |x::y::xs -> 
                let map = (addMoveToMap x map)
                let resultingWord = getPerpendicularWord map (fst x) horisontal
                let acc = isWord (resultingWord) dict
                traverseUntillLastLetterAndVerifyOrtogonalWords acc map (y::xs) dict horisontal
            |x::xs -> 
                let map = (addMoveToMap x map)
                let resultingWord = getPerpendicularWord map (fst x) (not horisontal)

                isWord (resultingWord) dict
            |_ -> acc

    let dict list = 
            list|>
            Seq.fold (fun acc x ->  insert x acc) (empty " ")
         
    let isHorisontalMove (move:(coord * char) list) =
        let xDifference = fst (fst move.[0]) - fst (fst move.[1]) 
        let yDifference = snd (fst move.[0]) - snd (fst move.[1])
        if xDifference <> 0 && yDifference = 0 
        then true
        else false
        
    let getXorYByHorisontal horisontal coord=
        if horisontal then fst coord else snd coord

    let isContinuosMove (move:(coord * char) list) horisontal =
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
        ) (true,fst move.[1]) move |>
        fst
    //            let move = List.map (fun x -> (fst x,fst (snd (snd x)))) move
    let isValidPlay (move:(coord * char) list) (board:board) dict : bool =
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
                            then false 
                            else true
            ) false move
        //establish direction
        let horisontal = isHorisontalMove move
        //check if continuos
        let isContinous = isContinuosMove move horisontal
        if (not includesHoles) && isContinous 
        then 
        //check for changed words by move
        //if hori then only check horisontally on the last tile and vice versa
        //for each tile check the ortogonal directions of the placement direction and verify that it creates valid words
            traverseUntillLastLetterAndVerifyOrtogonalWords true board.boardMap move dict horisontal
        else 
            false
    
    let boardProgToBoardFun boardProg squares  : boardFun= 
      runTextParser stmParse boardProg |> 
      stmntToBoardFun
      <| squares
    
    let squaresOfProgToSquaresOfFun squares =
     Map.map (
         fun id map ->
             Map.map (
                 fun key value -> 
                     runTextParser stmParse value |>
                     stmntToSquareFun
             ) map
     ) squares

    let boardToStateBoard (boardP:ScrabbleUtil.boardProg) map = 
        let squares = boardP.squares |> squaresOfProgToSquaresOfFun
        let boardFun = boardProgToBoardFun boardP.prog squares
        mkBoard boardFun boardP.usedSquare squares boardP.center map

    let boardToStateBoardWithMap (boardP:ScrabbleUtil.boardProg)= 
        boardToStateBoard boardP Map.empty

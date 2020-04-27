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
    let rec traverseUntillNull acc (map:Map<coord,char * int>) coord horisontal up =
        let result = map.TryFind coord
        if (result).IsSome 
        then 
            let coord = changeCoordAccordingToHorisontalAndUp coord horisontal up
            traverseUntillNull (result.Value :: acc) map coord horisontal up
        else List.rev acc

    let getPerpendicularWord map x horisontal up :string =
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
    //            let move = List.map (fun x -> (fst x,fst (snd (snd x)))) move
    let isValidPlay (move:(coord * (char * int)) list) (board:board) dict horisontal up: bool =
        //check if coords are not in holes
        let map = board.boardMap
        let boardFun = board.boardFun
        let includesHoles = 
            List.fold (
                fun acc x -> 
                    if acc 
                    then acc 
                    else 
                        (boardFun (fst x)) |>
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
            //check for changed words by move
            //if hori then only check horisontally on the last tile and vice versa
            //for each tile check the ortogonal directions of the placement direction and verify that it creates valid words
                traverseUntillLastLetterAndVerifyOrtogonalWords true board.boardMap move dict horisontal up
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

    //let convertFromServerMoveToVerifyMove (move:move) : (coord * char) list=

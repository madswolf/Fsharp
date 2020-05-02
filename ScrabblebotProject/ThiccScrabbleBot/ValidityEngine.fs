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

    //A function that traverses the given map horisontally or vertically positively or negatively depending on arguments
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

    let rec accumulateOrtogonalMoves acc (map:Map<coord,char * int>) (move:(coord * (char * int)) list) (horisontal:bool) up : ((coord * (char * int)) list) list =
        match move with
        |x::y::xs -> 
            let x = x
            let map = (addMoveToMap x map)
            let resultingWord = getPerpendicularMove map (fst x) horisontal up
            let acc = resultingWord :: acc
            accumulateOrtogonalMoves acc map (y::xs)horisontal up
        |x::xs -> 
            let map = (addMoveToMap x map)
            let perpendicularMove = getPerpendicularMove map (fst x) horisontal up
            let resultingWord = getPerpendicularMove map (fst x) (not horisontal) up

            resultingWord :: perpendicularMove:: acc
        |_ -> acc

    let getPerpendicularWord map x horisontal up : string =
        let move = getPerpendicularMove map x horisontal up
        (move) |> List.fold (fun acc item -> acc + string (fst (snd item)) ) "" 
    
    let rec verifyCreatedWords acc (map:Map<coord,char * int>) (move:(coord * (char * int)) list) (dict:Dictionary) (horisontal:bool) up:bool =
        if (not acc)
        then acc
        else 
            match move with
            |x::y::xs -> 
                let map = (addMoveToMap x map)
                let resultingWord = getPerpendicularWord map (fst x) horisontal up
                let acc = isWord (resultingWord) dict
                verifyCreatedWords acc map (y::xs) dict horisontal up
            |x::xs -> 
                let map = (addMoveToMap x map)
                let perpendicularWord = getPerpendicularWord map (fst x) horisontal up
                let resultingWord = getPerpendicularWord map (fst x) (not horisontal) up
                let result = (isWord resultingWord dict) && (isWord perpendicularWord dict)
                result
            |_ -> acc

        
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
        if move.Length = 1 then true
        else 
            List.fold (isContinous
            ) (true,(fst move.[1])) move |>
            fst
    let convertToValidationMove move : (coord * (char * int)) list=
         List.map (fun x -> (fst x , snd(snd x))) move

    let isValidPlay (move:(coord * (char * int)) list) (board:board) dict horisontal up: bool =
        let isStartingMove = board.boardMap.Count = 0
        let isMoveOverCenter = (if isStartingMove then (isMoveOverCenter board.center move) else true)

        let includesHoles = 
            List.fold (
                fun acc x -> 
                    if acc 
                    then acc 
                    else 
                        (board.boardFun (fst x) board.boardMap) |>
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
                        (Map.tryFind (fst x) board.boardMap) |>
                        fun thing -> 
                            if thing.IsSome 
                            then true 
                            else false
            ) false move

        let isContinous = isContinuosMove move horisontal
        if (not includesHoles) && (not overlapsWithExistingTiles) && isContinous
        then 
            if isStartingMove 
            then 
                if isMoveOverCenter
                then verifyCreatedWords true board.boardMap move dict horisontal up
                else false
            else 
                verifyCreatedWords true board.boardMap move dict horisontal up
        else 
            false

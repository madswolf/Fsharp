module Util
    open Ass7.ImpParser
    open Dictionary
    open State
    open Eval
    open ScrabbleUtil
    type Direction =                
    | Right  
    | Left
    | Up
    | Down

    //false false = right
    //true true = left
    //false true = up
    //true false = down
    let establishDirection (move:(coord * char) list) : (bool * bool) = 
        let move = List.map (fun x ->(fst x) ) move
        match move with
        |x::y::xs -> 
             if (fst y - fst x) > 0 then (false,false)
             else 
                if (fst y - fst x) < 0 then (true,true)
                else
                    if(snd y - snd x) > 0 then (false,true)
                    else (true,false)
        | _ -> failwith "can't establish"

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
        else acc

    let getPerpendicularWord map x horisontal =
        
        let coord = changeCoordAccordingToHorisontalAndUp  x (not horisontal) 1
        let thing = traverseUntillNull [] map coord (not horisontal) 1 |> List.rev
        let resultingword = (traverseUntillNull thing map  x (not horisontal) -1 )
        (if not horisontal then resultingword else List.rev resultingword) |> List.fold (fun acc item -> acc + string item ) "" 
         

    
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
         
    
    let isValidPlay (move:(coord * (uint32 * (char * int))) list) (board:board) : bool =
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
                            then thing.Value.IsEmpty 
                            else false
            ) false move
        if includesHoles 
        then false
        //check for changed words by move
        //check hori or verti if hori then only check horisontally on the last tile and vice versa
        //for each tile check the ortogonal directions of the placement direction and verify that it creates valid words
    
        else true
    
    
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
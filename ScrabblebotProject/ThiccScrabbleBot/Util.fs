module Util
    open Ass7.ImpParser
    open Dictionary
    open JParsec.TextParser
    open State
    open Eval
    open ScrabbleUtil
    open JParsec
    
    
    //A function that traverses the given map horisontally or vertically positively or negatively depending on arguments
    let rec traverseUntillNull acc (map:Map<coord,char>) coord horisontal up=
        let result = map.TryFind coord
        if (result).IsSome 
        then 
            let coord = if horisontal then (((fst coord) + up), snd coord) else (fst coord , (snd coord) + up)
            traverseUntillNull (result.Value :: acc) map coord horisontal up
        else acc
    
    let rec traverseUntillLastLetterAndVerifyOrtogonalWords acc (map:Map<coord,char>) (move:(coord * char) list) (dict:Dictionary) (horisontal:bool) :bool =
        if (not acc)
        then acc
        else 
            match move with
            |x::xs -> 
                let thing = traverseUntillNull [] map (fst x) (not horisontal) 1
                let acc = lookup (string (traverseUntillNull thing map (fst x) (not horisontal) -1)) dict
                traverseUntillLastLetterAndVerifyOrtogonalWords acc map xs dict horisontal
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
      (
         function
         |Success (x,_) -> x 
         |_ -> failwith "oopps"
      ) |>
      stmntToBoardFun
      <| squares
    
    let squaresOfProgToSquaresOfFun squares =
     Map.map (
         fun id map ->
             Map.map (
                 fun key value -> 
                     runTextParser stmParse value |>
                     (
                         function
                         |Success (x,_) -> x 
                         |_ -> failwith "oopps"
                     ) |>
                     stmntToSquareFun
             ) map
     ) squares
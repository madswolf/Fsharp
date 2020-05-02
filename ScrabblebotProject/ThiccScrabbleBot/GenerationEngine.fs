module GenerationEngine

    open System
    open Ass7.ImpParser
    open Dictionary
    open State
    open Eval
    open ValidityEngine
    open ScrabbleUtil
    open MultiSet
    open FSharp.Collections.ParallelSeq
    //returns a list of string * coord * bool where the strings are the prefixes, 
    //the coord is the coord of the last character, and the boolean is if it's horisontal or not
    let findPrefixes map dict up =
       
        let isEndOfWord coord map horisontal =
            let coord1 = changeCoordAccordingToHorisontalAndUp coord horisontal up
            ((Map.tryFind coord1 map).IsSome)

        let findAndAddPrefix (coord:coord) map horisontal acc = 
            if (not (isEndOfWord coord map horisontal)) 
            then 
                let prefix = getPerpendicularWord map coord (not horisontal) up
                if (Dictionary.isContinuable prefix dict) 
                then ((prefix, coord),horisontal)::acc 
                else (acc)
            else acc

        let tryFindHoriVerti acc coord map =
             let acc = findAndAddPrefix coord map true acc
             findAndAddPrefix coord map false acc
        
        Map.fold (fun acc key _ -> tryFindHoriVerti acc key map ) [] map
    
    //this used to be an aux function within attemptAllCombinations, but i realised that it was needed for parallel moves
    let rec attemptAllCombinationsInner (acc:(move * bool) list) (move:move) (hand:MultiSet<uint32>) dict coord horisontal state moveValidator coordChanger up : (move * bool) list= 
        //try all sets
         let handList = MultiSet.toList hand
         if handList.Length = 0
         then acc
         else
             List.fold (fun acc tileID -> 
                 let tile = Map.find tileID state.tiles
                 //try all chars in set
                 Set.fold (fun acc tileValue -> 
                     let tileAtCoord = Map.tryFind coord state.board.boardMap
                     let char = (fst tileValue) 
                     let value = (snd tileValue)
                     let finding = Dictionary.tryFind (fst tileValue) dict
                     if finding.IsSome && tileAtCoord.IsNone
                     then 
                        let newMove = (coord, (tileID,(char,value))) :: move
                        let newHand = (MultiSet.removeSingle tileID hand)
                        let dict = finding.Value
                        
                        if isWordDict dict && moveValidator (convertToValidationMove newMove) up
                        then 
                            if Dictionary.isContinuableDict dict
                            then attemptAllCombinationsInner ((newMove, horisontal) :: acc) newMove newHand dict (coordChanger coord) horisontal state moveValidator coordChanger up
                            else (newMove,horisontal) :: acc
                        else attemptAllCombinationsInner acc newMove newHand dict (coordChanger coord) horisontal state moveValidator coordChanger up
                     else
                         if 
                            //bridge move
                            tileAtCoord.IsSome 
                            && (traverseUntillNullAccumulateMove [] state.board.boardMap coord horisontal up |> List.fold (fun acc item -> acc + string (fst(snd item))) "" ) |> isWord  <|dict
                            && moveValidator (convertToValidationMove move) up
                         then  (move,horisontal) :: acc
                         else acc
                 ) acc tile 
             ) acc handList

    let attemptAllCombinations state (acc:(move * bool) list) (prefix:((string * coord) * bool)) (hand:MultiSet<uint32>) dict up : (move * bool) list =     

               let word = (fst (fst prefix))
               let horisontal = snd prefix

               let coord = snd(fst prefix )
               
               let coordChanger = 
                   fun coord -> 
                       changeCoordAccordingToHorisontalAndUp coord horisontal up

               let moveValidator move up =
                   let isValid = isValidPlay move state.board dict horisontal up
                   isValid

               let dict = 
                   if word.Length <> 0 
                   then 
                       (Dictionary.tryFindString word dict).Value
                   else dict
               
               attemptAllCombinationsInner acc [] hand dict (coordChanger coord) horisontal state moveValidator coordChanger up


    let generateAllValidMoves (state:state) : (move* bool) list=

        let map = state.board.boardMap
        let dict = state.dictionary
        let revDict = state.reverseDictionary
        let up = 1
        let negUp = -1
        let hand = 
            state.hand 
        
        let isStartingMove = map.Count = 0 
        let prefixes = 
            if isStartingMove
            //if you have the starting hand
            then 
                [
                (("",changeCoordAccordingToHorisontalAndUp state.board.center true negUp),true);
                (("",changeCoordAccordingToHorisontalAndUp state.board.center false negUp),false)
                ]
            else findPrefixes map dict up
        let suffixes = 
            if isStartingMove
            then 
                [
                (("",changeCoordAccordingToHorisontalAndUp state.board.center true up),true);
                (("",changeCoordAccordingToHorisontalAndUp state.board.center false up),false)
                ]
            else
                findPrefixes map revDict negUp

        let result = 
            PSeq.collect (fun prefix -> (attemptAllCombinations state [] prefix hand dict up)) prefixes |>
            PSeq.toList
        (PSeq.collect (fun suffix -> attemptAllCombinations state [] suffix hand revDict negUp) suffixes |>
         PSeq.toList) @ result


    let generateParrallelMovesWithMovePrefix acc (state:state) (move:(move * bool)): (move * bool) list = 
        let index = (fst move).[0]
        let hand = MultiSet.removeSingle (fst (snd index)) state.hand
        let startCoord = (fst index)
        let horisontal = (not (snd move))
        
        let moveValidator move up =
            let isValid = isValidPlay move state.board state.dictionary horisontal up
            isValid
        let coordChanger = 
            fun coord -> 
                changeCoordAccordingToHorisontalAndUp coord horisontal 1
        let continueCoord = coordChanger startCoord
        let dict = (Dictionary.tryFind (fst (snd(snd index))) state.dictionary).Value
        let result = attemptAllCombinationsInner acc (fst move) hand dict continueCoord horisontal state moveValidator coordChanger 1

        let moveValidator move up =
            let isValid = isValidPlay move state.board state.reverseDictionary horisontal up
            isValid
        let coordChanger = 
            fun coord -> 
                changeCoordAccordingToHorisontalAndUp coord horisontal -1
        let continueCoord = coordChanger startCoord
        let dict = (Dictionary.tryFind (fst (snd(snd index))) state.reverseDictionary).Value
        attemptAllCombinationsInner result (fst move) hand dict continueCoord horisontal state moveValidator coordChanger -1 

    let getSquaresForMove state move =
        List.map(
            fun x -> 
                let result = state.board.boardFun (fst x) state.board.boardMap
                if result <> -1
                then
                    state.board.squares.[result]
                else []
        ) move

    let calculatepointsForMove state (move: (move * bool)) : move * int =
        let handSize = state.handSize
        let thing =
            List.map(fun thing ->(fst thing, (snd(snd thing)))) (fst move) |>
            (fun x -> accumulateOrtogonalMoves [] state.board.boardMap x (snd move) 1) |>
            List.filter (fun x ->  x.Length <> 1) |>
            List.fold (
                fun acc x -> 
                    let word =  x |> List.map (snd)
                    let squares = getSquaresForMove state x
                    let points = calculatePoints squares word 
                    points + acc
            ) 0
        let result =
            if (fst move).Length = handSize 
            then (thing + 50)
            else thing
        (fst move,result)

    let generateMostCashMoneyMoveParallel (state:state) : move =
        
        let moves = generateAllValidMoves state

        let movesWithParallel = 
            moves |> 
            List.filter (fun x -> (fst x).Length = 1) |>
            List.fold (fun acc x -> generateParrallelMovesWithMovePrefix acc state x) moves

        let movesSortedByPoints = 
            PSeq.map (fun x ->calculatepointsForMove state x) movesWithParallel |>
            PSeq.toList |>
            List.sortBy (fun x -> snd x) |> 
            List.rev

        if movesSortedByPoints.Length <> 0
        then
            fst movesSortedByPoints.[0]
        else []
            

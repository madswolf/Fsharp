module GenerationEngine

    open System
    open Ass7.ImpParser
    open Dictionary
    open State
    open Eval
    open ValidityEngine
    open ScrabbleUtil
    open MultiSet
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


    let generateAllValidMoves (state:state) : move list=

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
        
        let attemptAllCombinations  (acc:move list) (prefix:((string * coord) * bool)) (hand:MultiSet<uint32>) dict up : move list =     

            let word = (fst (fst prefix))
            let horisontal = snd prefix

            let coord = snd(fst prefix )
            
            let coordChanger = 
                fun coord -> 
                    changeCoordAccordingToHorisontalAndUp coord horisontal up

            let isMoveOverCenter move =
                List.fold (fun acc item -> if acc then acc else fst item = state.board.center) false move

            let isMoveValid move up =
                let isValid = isValidPlay move state.board dict horisontal up
                let moveCenter = (if isStartingMove then (isMoveOverCenter move) else true)
                isValid && moveCenter

            let dict = 
                if word.Length <> 0 
                then 
                    (Dictionary.tryFindString word dict).Value
                else dict

            let rec aux (acc:move list) (move:move) (hand:MultiSet<uint32>) dict coord: move list= 
                //try all sets
                 let handList = MultiSet.toList hand
                 List.fold (fun acc tileID -> 
                     let tile = Map.find tileID state.tiles
                     //try all chars in set
                     Set.fold (fun acc tileValue -> 
                         let tileAtCoord = Map.tryFind coord map
                         let char = (fst tileValue) 
                         let value = (snd tileValue)
                         let finding = Dictionary.tryFind (fst tileValue) dict
                         if finding.IsSome && tileAtCoord.IsNone
                         then 
                            let newMove = (coord, (tileID,(char,value))) :: move
                            let newHand = (MultiSet.removeSingle tileID hand)
                            let dict = finding.Value
                            
                            if isWordDict dict && isMoveValid (convertToValidationMove newMove) up
                            then newMove :: acc
                            else aux acc newMove newHand dict (coordChanger coord)
                         else
                             if 
                                tileAtCoord.IsSome 
                                && (traverseUntillNull [] map coord horisontal up |> List.fold (fun acc item -> acc + string (fst item)) "" ) |> isWord  <|dict
                                && isMoveValid (convertToValidationMove move) up
                             then  move :: acc
                             else acc
                     ) acc tile 
                 ) acc handList
            aux acc [] hand dict (coordChanger coord)
        let result = List.fold (fun acc prefix -> attemptAllCombinations acc prefix hand dict up) [] prefixes
        List.fold (fun (acc:move list) suffix -> attemptAllCombinations acc suffix hand revDict negUp) result suffixes

    let generateSortaMostCashMoneyMove (state:state) : move =
        let moves = generateAllValidMoves state |> List.sortBy (fun x -> x.Length) |> List.rev
        if moves.Length <> 0
        then 
            moves.[0]
        else []

    let getSquaresForMove state move =
        List.map(
            fun x -> 
                let result = state.board.boardFun (fst x) state.board.boardMap
                if result <> -1
                then
                    state.board.squares.[result]
                else []
        ) (fst move)


    let calculatepointsForCreatedMoves state (move: move) : move * int =
        let handSize = int (MultiSet.size (state.hand))
        let thing =
            List.map(fun thing ->(fst thing, (snd(snd thing)))) move |>
            (fun x -> traverseUntillLastLetterAndAccumulateOrtogonalMoves [] state.board.boardMap x true 1) |>
            List.fold (
                fun acc x -> 
                    let squares = getSquaresForMove state x
                    let word = snd x 
                    let points = calculatePoints squares word 
                    points + acc
            ) 0
        let result =
            if move.Length = handSize 
            then (thing + 50)
            else thing
        (move,result)


    let generateMostCashMoneyMove (state:state) : move =
        
        let moves = generateAllValidMoves state
            
        let movesSortedByPoints = 
            List.map (fun x -> calculatepointsForCreatedMoves state x )  moves |>
            List.sortBy (fun x -> snd x) |> 
            List.rev

        if movesSortedByPoints.Length <> 0
        then
            fst movesSortedByPoints.[0]
        else []

    let generateMostCashMoneyMoveParallel (state:state) : move =
        
        let moves = generateAllValidMoves state

        let movesSortedByPoints = 
            List.map (fun x -> async{ return calculatepointsForCreatedMoves state x}) moves |>
            Async.Parallel |>
            Async.RunSynchronously |>
            Array.toList |> 
            List.sortBy (fun x -> snd x) |> 
            List.rev

        if movesSortedByPoints.Length <> 0
        then
            fst movesSortedByPoints.[0]
        else []
            
                

    let generateValidMove (state:state) : move=

        let map = state.board.boardMap
        let dict = state.dictionary
        let revDict = state.reverseDictionary
        let up = 1
        let negUp = -1
        let hand = 
            state.hand |>
            MultiSet.toList |>
            List.fold(fun acc item -> Set.add ((item, Map.find item state.tiles)) acc) Set.empty
        
        let isStartingMove = map.Count = 0 
        let prefixes = 
            if isStartingMove
            //if you have the starting hand
            //add more ending locations for this, and check if the move is over the center 
            then 
                [
                (("",changeCoordAccordingToHorisontalAndUp state.board.center true negUp),true);
                (("",changeCoordAccordingToHorisontalAndUp state.board.center false negUp),false)
                ]
            else findPrefixes map dict up
        //prefix + hand 
        let suffixes = 
            if isStartingMove
            then 
                [
                (("",changeCoordAccordingToHorisontalAndUp state.board.center true up),true);
                (("",changeCoordAccordingToHorisontalAndUp state.board.center false up),false)
                ]
            else
                findPrefixes map revDict negUp
        //includes destructive updates I.E
        //if a tile in hand can move toward a complete word then it appends it to the string
        //and removes that tile from the pool
        //then calls itself with these 



            //each iteration ask for each char in hand (set list) if appended is a word or continuable
            //each set has a list of chars that are contiunable and the set itself 
            //then have a function that runs through each char in that list and call a new iteration where 
            //let chars = MultiSet.map (fun item -> )
            
        //run through all prefixes and try each combination of tiles in hand 
        
        let attemptAllCombinations  (prefix:((string * coord) * bool)) (hand:Set<uint32 * tile>) dict up : move =     

            let word = (fst (fst prefix))
            let horisontal = snd prefix

            let coord = snd(fst prefix )
            
            let coordChanger = 
                fun coord -> 
                    changeCoordAccordingToHorisontalAndUp coord horisontal up

            let isMoveOverCenter move =
                List.fold (fun acc item -> if acc then acc else fst item = state.board.center) false move

            let isMoveValid move up =
                let isValid = isValidPlay move state.board dict horisontal up
                let moveCenter = (if isStartingMove then (isMoveOverCenter move) else true)
                isValid && moveCenter

            let dict = 
                if word.Length <> 0 
                then 
                    (Dictionary.tryFindString word dict).Value
                else dict

            let rec aux (move:move) (word:string) (hand:Set<uint32 * tile>) dict coord: move = 
                    //try all sets
                     Set.fold (
                         fun acc tile -> 
                             if acc <> [] 
                             then acc 
                             else 
                                 //try all chars in set
                                 Set.fold (
                                     fun acc tileValue -> 
                                         if acc <> []
                                         then acc
                                         else 
                                             //if a char in the set has a continuation then call it again with new dict, 
                                             //new word and remove the tile from the hand
                                             let tileAtCoord = Map.tryFind coord map
                                             let finding = Dictionary.tryFind (fst tileValue) dict
                                             if finding.IsSome && tileAtCoord.IsNone
                                             then 
                                                let newMove = (coord, ((fst tile),(fst tileValue,snd tileValue))) :: move
                                                let newHand = (Set.remove ((fst tile),snd tile) hand)
                                                let dict = finding.Value
                                                if isWordDict dict && isMoveValid (convertToValidationMove newMove) up
                                                then 
                                                    newMove
                                                else
                                                    aux newMove (word + (string char)) newHand dict (coordChanger coord)
                                             //if not
                                             else
                                                 if 
                                                    tileAtCoord.IsSome 
                                                    && (traverseUntillNull [] map coord horisontal up |> List.fold (fun acc item -> acc + string (fst item)) "" ) |> isWord  <|dict
                                                    && isMoveValid (convertToValidationMove move) up
                                                 then 
                                                    //placing move would create a word
                                                    (printf "%A" move) |> ignore
                                                    move
                                                 else
                                                    acc
                                 ) acc (snd tile)
                                 
                     ) [] hand
            aux [] word hand dict (coordChanger coord)
            

        //See if combination is word is true
        let result = 
            List.fold (
                fun acc prefix -> 
                    if acc <> [] 
                    then acc 
                    else 
                        attemptAllCombinations prefix hand dict up
            ) [] prefixes
        if result = []
        then
            List.fold (
                fun acc suffix -> 
                    if acc <> [] 
                    then acc 
                    else 
                        attemptAllCombinations suffix hand revDict negUp
            ) [] suffixes
        else result

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
            
            let attachCoordsTomove (move:(uint32 *(char * int)) list) : move=
                let coordChanger = 
                    fun upamount -> 
                        let coord = snd (fst prefix)
                        changeCoordAccordingToHorisontalAndUp coord horisontal (upamount * up)
                let coords = 
                    [1 ..  move.Length] |>
                    List.map (coordChanger)
                List.zip coords move

            let isMoveOverCenter move =
                List.fold (fun acc item -> if acc then acc else fst item = state.board.center) false move

            let isMoveValid move up =
                let moveWithCoords = 
                    attachCoordsTomove (move |> List.rev) |>
                    List.map (fun item -> (fst item, fst(snd(snd item))))
                let isValid = isValidPlay moveWithCoords state.board dict up
                let moveCenter = (if isStartingMove then (isMoveOverCenter moveWithCoords) else true)
                isValid && moveCenter
            let dict = 
                if word.Length <> 0 
                then 
                    (Dictionary.tryFindString word dict).Value
                else dict

            let rec aux (move: (uint32 *(char * int)) list) (word:string) (hand:Set<uint32 * tile>) dict up: move = 
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
                                             let finding = Dictionary.tryFind (fst tileValue) dict
                                             if finding.IsSome
                                             then 
                                                let newMove = ((fst tile),(fst tileValue,snd tileValue)) :: move
                                                let newHand = (Set.remove ((fst tile),snd tile) hand)
                                                let dict = finding.Value
                                                if isWordDict dict && isMoveValid newMove up
                                                then 
                                                    attachCoordsTomove (newMove |> List.rev)
                                                else
                                                    aux newMove (word + (string char)) newHand dict up
                                             //if not
                                             else 
                                                 acc
                                 ) acc (snd tile)
                                 
                     ) [] hand
            aux [] word hand dict up
            

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
        //See if play is valid
    
        //if valid return that move
    
        //(word,horisontal)

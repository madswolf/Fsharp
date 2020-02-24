module Dictionary
    open System

    type Dictionary = Dictionary of Map<char,Dictionary> * bool
        


    let empty (alphabet:string) :Dictionary = 
        Dictionary(Map.empty,true)


    let rec insert (word:string) (dict:Dictionary) : Dictionary =
        let (Dictionary(map,isWord)) = dict
        if(word.Length = 0) then dict
        else
            if((map.TryFind word.[0]).IsNone) 
            then 
                let isCompleted = if(word.Length = 1) then true else false 
                let newDictWithLetter = Dictionary(map.Add (word.[0], empty ""), isCompleted)
                let innerDictWithRestOfWord = insert word newDictWithLetter
                innerDictWithRestOfWord
            else
                let innerDictCorrespondingToLetter = map.[word.[0]]
                let innerDictWithRestOfWord = insert word.[1..word.Length-1] innerDictCorrespondingToLetter 
                Dictionary(map.Add(word.[0],innerDictWithRestOfWord),false)
    
    let rec lookup (word:string) (dict:Dictionary) : bool =
        if(word.Length = 0) then true
        else
            if((dict.map.TryFind word.[0]).IsNone) then false 
            else lookup word.[1..word.Length-1] dict.map.[word.[0]]
        

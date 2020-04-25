module Dictionary
open System

type Dictionary = Dictionary of  Map<char,Dictionary> * bool

let empty (alphabet:string) :Dictionary = 
    Dictionary(Map.empty,false)


let rec insert (word:string) (dict:Dictionary) : Dictionary =
    match dict with
    |Dictionary(map,isWord) when word.Length = 0 -> Dictionary(map,true)
    |Dictionary(map,isWord) -> 
        let innerDict = insert word.[1..] (if map.ContainsKey word.[0] then map.[word.[0]] else empty "")
        Dictionary(map.Add(word.[0],innerDict),isWord)

let mkDict (words:string list) =
    List.fold (fun dict item -> insert item dict) (empty "") words

let rec lookup (word:string) (dict:Dictionary) : bool =
    let (Dictionary(map,isWord)) = dict
    if(word.Length = 0) then isWord
    else
        if((map.TryFind word.[0]).IsNone) then false
        else lookup word.[1..word.Length-1] map.[word.[0]]


let isWord  (word:string) (dict:Dictionary) : bool = 
    if (word.Length = 0 || word.Length = 1) then true else lookup word dict

let rec isContinuable (word:string) dict =
    let (Dictionary(map,_)) = dict
    if(word.Length = 0 && map.Count <> 0) then true
    else
        if (word.Length = 0) then false 
        else 
            if((map.TryFind word.[0]).IsNone) then failwith "isNotWord"
            else isContinuable word.[1..word.Length-1] map.[word.[0]]

let tryFind char dict =
    let (Dictionary(map,isWord)) = dict
    map.TryFind char

let isWordDict dict =
    let (Dictionary(map,isWord)) = dict
    isWord

let rec tryFindString (word:string) dict =
    let (Dictionary(map,isWord)) = dict
    if (word.Length = 1) then Map.tryFind word.[0] map 
    else 
        if (Map.tryFind word.[0] map).IsNone then Map.tryFind word.[0] map
        else tryFindString word.[1..word.Length-1] map.[word.[0]]

let ofList dict =
    let rec aux acc string dict =
        let (Dictionary(map,isWord)) = dict
        let acc = if isWord then string::acc else acc
        Map.fold (fun acc key value -> aux acc (string + key) value) acc map
    aux [] dict


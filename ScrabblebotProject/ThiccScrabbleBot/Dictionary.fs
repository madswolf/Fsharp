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


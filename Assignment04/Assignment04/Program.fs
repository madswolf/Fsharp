// Learn more about F# at http://fsharp.org

module MultiSet
    open System
    [<StructuredFormatDisplay("{AsString}")>]
    type MultiSet<'a> when 'a : comparison = MultiSet of  Map<'a,uint32> * uint32
    with 
        override this.ToString() =
            let func = fun acc key value -> acc + "(" + key.ToString() + ", #" + value.ToString() + "), "
            let (MultiSet(map,size)) = this
            let content = (Map.fold (func) "" map)
            sprintf "{" + content.[0..content.Length-3] + "}"
        member this.AsString = this.ToString()

    let empty : MultiSet<'a> = MultiSet(Map.empty, 0u)

    let isEmpty (set:MultiSet<'a>) : bool = 
        let (MultiSet(map,size)) = set
        size = 0u

    let size (set:MultiSet<'a>) : uint32 =
        let (MultiSet(map,size)) = set
        size

    let contains (key:'a) (set:MultiSet<'a>) : bool =
        let (MultiSet(map,size)) = set
        (map.TryFind key).IsSome 

    let numItems (key:'a) (set:MultiSet<'a>) : uint32 =
        let (MultiSet(map,size)) = set
        if (map.TryFind key).IsSome then map.[key] else 0u 

    let add (key:'a) (count:uint32) (set:MultiSet<'a>) : MultiSet<'a> =
        let (MultiSet(map,size)) = set
        if (map.TryFind key).IsSome 
            then MultiSet(map.Add(key, (map.[key] + count)), size + count)
            else MultiSet(map.Add(key, count), size + count)

    let addSingle (key:'a) (set:MultiSet<'a>) : MultiSet<'a> =
        add key 1u set
    let test1 = empty |> add 'a' 2u |> add 'b' 3u

    let set = empty |> add "a" 2u |> add "b" 1u |> add "c" 3u |> add "y" 5u;;

    let remove (key:'a) (count:uint32) (set:MultiSet<'a>) : MultiSet<'a> =    
        let (MultiSet(map,size)) = set
        if(map.TryFind key).IsSome 
            then 
                if map.[key] <= count 
                    then MultiSet(map.Remove(key), size - map.[key])
                    else MultiSet(map.Add(key,(map.[key] - count)), size - count)
            else set

    let removeSingle (key:'a) (set:MultiSet<'a>) : MultiSet<'a> =
        remove key 1u set

    let fold (func:'a -> 'b -> uint32 -> 'a) (acc:'a) (set:MultiSet<'b>) : 'a = 
        let (MultiSet(map,size)) = set
        Map.fold func acc map

    let foldBack (func:'a -> uint32 -> 'b -> 'b) (set:MultiSet<'a>) (acc:'b) : 'b =
        let (MultiSet(map,size)) = set 
        Map.foldBack func map acc

    let map (func:'a -> 'b) (set:MultiSet<'a>) : MultiSet<'b> =
        let (MultiSet(map,size)) = set
        Map.fold (fun acc key value -> add (func key) value acc ) empty map
        

    let ofList (list:'a list) : MultiSet<'a> =
        List.fold (fun acc item -> addSingle item acc) empty list

    let toList (set:MultiSet<'a>) : 'a list = 
        let func = fun acc key value -> List.fold (fun innerAcc item -> key :: innerAcc) acc [1u..value]
        List.rev (fold (func) [] set)

    //less complex but worse performance, as replicate is linear to n, and so is @. So it's O(2n) and not O(n) like the previous
    let toListAlt (set:MultiSet<'a>) : 'a list = 
        let func = fun acc key (value:uint32) -> (List.replicate (int value) key) @ acc
        List.rev (fold (func) [] set)
    
    let set2 = empty |> add "a" 5u |> add "g" 8u |> add "c" 2u |> add "y" 6u;;

    let union (set1:MultiSet<'a>) (set2:MultiSet<'a>) : MultiSet<'a> =

         //remove intances of overlap where set1 has less
        let remainder = fold (fun acc key value -> if(numItems key set2 < value) then add key value acc else acc ) empty set1
         //remove intances of overlap where set2 has less
        let remainder2 = fold (fun acc key value -> if(numItems key set1 < value) then add key value acc else acc ) empty set2

        // add the remainder of set1 and set2 together and return
        fold (fun acc key value -> add key value acc) remainder remainder2 

    let sum (set1:MultiSet<'a>) (set2:MultiSet<'a>) : MultiSet<'a> =
        fold (fun acc key value ->   add key value acc) set2 set1

    let subtract (set1:MultiSet<'a>) (set2:MultiSet<'a>) : MultiSet<'a> =
        fold (fun acc key value ->   remove key value acc) set1 set2

    let intersection (set1:MultiSet<'a>) (set2:MultiSet<'a>) : MultiSet<'a> =
        fold (
            fun acc key value ->  
                if(contains key set2) 
                then 
                    if(numItems key set2 > value) 
                    then add key value acc 
                    else add key (numItems key set2) acc
                else acc) empty set1


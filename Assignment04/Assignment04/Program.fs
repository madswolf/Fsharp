// Learn more about F# at http://fsharp.org

module MultiSet
    open System
    type MultiSet<'a> when 'a : comparison = MultiSet of Map<'a, uint32> * uint32

    let empty : MultiSet<'a> = MultiSet(Map.empty,0u)

    let isEmpty (set:MultiSet<'a>) : bool = 
        let (MultiSet(map, size)) = set
        size = 0u

    let size (set:MultiSet<'a>) : uint32 =
        let (MultiSet(map, size)) = set
        size

    let contains (key:'a) (set:MultiSet<'a>) : bool =
        let (MultiSet(map, size)) = set
        (map.TryFind key).IsSome 

    let numItems (key:'a) (set:MultiSet<'a>) : uint32 =
        let (MultiSet(map, size)) = set
        if (map.TryFind key).IsSome then map.[key] else 0u 

    let add (key:'a) (count:uint32) (set:MultiSet<'a>) : MultiSet<'a> =
        let (MultiSet(map, size)) = set
        if (map.TryFind key).IsSome 
            then MultiSet(map.Add(key, (map.[key] + count)),size + count) 
            else MultiSet(map.Add(key,1u),size + count)

    
    let addSingle (key:'a) (set:MultiSet<'a>) : MultiSet<'a> =
        add key 1u set

    let remove (key:'a) (count:uint32) (set:MultiSet<'a>) : MultiSet<'a> =    
        let (MultiSet(map, size)) = set
        if(map.TryFind key).IsSome 
            then 
                if map.[key] < count 
                    then MultiSet((map.Remove key),size - map.[key]) 
                    else MultiSet(map.Add(key , (map.[key] - count)),size - count)
            else set

    let removeSingle (key:'a) (set:MultiSet<'a>) : MultiSet<'a> =
        remove key 1u set



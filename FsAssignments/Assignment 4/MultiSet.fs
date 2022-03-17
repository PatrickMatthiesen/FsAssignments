
module MultiSet
    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>
    let empty = MS Map.empty
    let isEmpty (MS s) =  s.IsEmpty
    let size (MS s) = Map.fold (fun acc _ item -> acc + item) 0u s
    let contains key (MS s) = s.ContainsKey key
    let numItems key (MS s) = s.TryFind key |> Option.defaultValue 0u
    let add key n (MS s) = MS (s.Add (key, (numItems key (MS s)+n)))
    let addSingle key (s: MultiSet<'a>) = add key 1u s
    let remove key n (MS s) = 
        let value = numItems key (MS s)
        if not (value = 0u) && value > n
        then MS (s.Add (key, value-n))
        else MS (s.Remove key)
    let removeSingle key (s: MultiSet<'a>) = remove key 1u s
    let fold f acc (MS s) = Map.fold f acc s
    let foldBack f (MS s) acc = Map.foldBack f s acc
    let ofList lst = List.fold (fun acc item -> addSingle item acc) empty lst
    let toList (s: MultiSet<'a>) = fold (fun acc key item -> acc@[for _ in 1..(int item) -> key]) [] s
    let map f (s: MultiSet<'a>) = fold (fun acc key item -> add (f key) item acc) empty s
    let union (s1: MultiSet<'a>) (s2: MultiSet<'a>) = 
            (s1, s2) ||> fold (fun acc key item -> 
                let accItem = numItems key acc
                if item > accItem
                then add key (item-accItem) acc
                else acc)
    let sum (s1: MultiSet<'a>) (s2: MultiSet<'a>) = (s1, s2) ||> fold (fun acc key item -> add key item acc)
    let subtract (s1: MultiSet<'a>) (s2: MultiSet<'a>) = (s1, s2) ||> fold (fun acc key item -> remove key item acc)
    let intersection (s1: MultiSet<'a>) (s2: MultiSet<'a>) = 
            (empty, s2) ||> fold (fun acc key item -> 
                match numItems key s1 with
                | 0u -> acc
                | s2Item when s2Item < item -> add key s2Item acc
                | _ -> add key item acc)


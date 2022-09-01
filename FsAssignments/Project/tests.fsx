#load "Gaddag.fs"
#load "..\Assignment 4\MultiSet.fs"

//let _split (s:string) = 
//    [for i in 0..(s.Length-1) -> 
//        (s.Substring(0,i+1) |> Array.ofSeq |> Array.rev |> System.String) + $">{s[i+1..s.Length-1]}"
//    ]

//_split "Hello"



let flatmap f = Option.map f >> Option.flatten

let testGaddag str gdag =
    let rec lookup =
        function
        | [] -> fun _ -> failwith "This can never happen"
        | [x] -> Gaddag.step x
        | x :: xs ->
        Gaddag.step x >>
        flatmap (snd >> lookup xs)

    let rec lookups acc back =
        function
        | [] -> fun _ -> Some false
        | [x] ->
            lookup (x :: back) >>
            flatmap (snd >> Gaddag.reverse) >>
            Option.map (fun (b, _) -> acc && b)
        | x :: xs ->
            lookup (x :: back) >>
            flatmap (snd >> Gaddag.reverse) >>
            flatmap (snd >> lookup xs) >>
            flatmap (fun (b, _) -> lookups (acc && b) (x :: back) xs gdag)
    gdag |>
    lookups true [] [for c in str -> c] |>
    (=) (Some true)

testGaddag "OD" (Gaddag.empty() |> Gaddag.insert "DO" |> Gaddag.insert "OD")

match Gaddag.empty() |> Gaddag.insert "DO" |> Gaddag.step 'D' with
    | Some (b, dict) -> Gaddag.step 'O' dict
    

//let idToChar : Map<uint32, (char*int)> = Map.fold (fun acc k (v: tile) -> if (v.Count = 1) then Set.fold (fun acc peice -> acc.Add (k, peice)) acc v else acc.Add (k,('!',0))) Map.empty tiles

//let getPermutations (st: Gaddag.Dict) (hand: MultiSet.MultiSet<uint32>) = 
//    let rec aux (dict: Gaddag.Dict) (hand: MultiSet.MultiSet<uint32>) (partialWord: ((char*int)*uint32) list) = 
//        MultiSet.toList hand |>
//        List.fold (fun acc id -> 
//            //TODO handle '!' as the joker tile
//            let (char,int) = idToChar[id]
//            match Gaddag.step char dict with
//            | Some (b, newDict) -> 
//                let s = (partialWord @ [((char, int), id)])
//                (if b then s :: acc else acc) @ aux newDict (MultiSet.removeSingle id hand) s
//            | None -> acc
//            ) List.empty

//    aux Gaddag. hand List.empty
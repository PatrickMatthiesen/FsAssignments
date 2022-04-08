#load "Gaddag.fs"

//let _split (s:string) = 
//    [for i in 0..(s.Length-1) -> 
//        (s.Substring(0,i+1) |> Array.ofSeq |> Array.rev |> System.String) + $">{s[i+1..s.Length-1]}"
//    ]

//_split "Hello"

'H'.ToString()


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

testGaddag "HELLO" (Gaddag.empty() |> Gaddag.insert "HELLO")
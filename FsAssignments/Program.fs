// For more information see https://aka.ms/fsharp-console-apps
//printfn "Hello from F#"

//open Dictionary

//let emptyDict = empty ()
//printfn "%A" (insert "hel" (insert "hello" emptyDict))

//Dictionary.lookup "DEKED" (Dictionary.empty () |> Dictionary.insert "DAD" |> Dictionary.insert "DEED" |> Dictionary.insert "DEIFLIED" |> Dictionary.insert "DEKED")

//Dictionary.lookup "DED" (Dictionary.empty () |> Dictionary.insert "DAD" |> Dictionary.insert "DEED")

//open Gaddag

//let emptyDict = empty ()

//insert "Hello" emptyDict |> printfn "%A"

//lookup "Hello" (insert "Hello" emptyDict) |> printfn "%A"

//Gaddag.empty() |> Gaddag.insert "DO" |> Gaddag.step 'D' |> Option.defaultValue (false, Gaddag.empty()) |> snd |> Gaddag.step 'O'


type element = string list

let elToString (el: element) : string =
    Seq.map string el |> String.concat ""

let elFromString (s: string) : element =
    [for c in s -> string c]
    
let nextElement (el: element) = 
    let rec aux (el: element) (acc: element) c =
        match el with
        | [] -> acc
        | x :: xs ->
            if x = c 
            then 
                printf "%A\n" x
                match acc with
                | [] -> aux xs [x] x
                | _ -> aux xs (x + acc.Head :: acc.Tail) x
            else aux xs (x :: acc) x

    aux el [] el.Head |> List.fold (fun acc a -> [for c in a.Length.ToString() -> string c] @ string a.[0] :: acc) []

printf "%A" ("111111111111111111111111111111111122222222222222222222222223331" |> elFromString |> nextElement |> nextElement |> elToString)
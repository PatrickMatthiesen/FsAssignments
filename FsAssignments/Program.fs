// For more information see https://aka.ms/fsharp-console-apps
//printfn "Hello from F#"

//open Dictionary

//let emptyDict = empty ()
//printfn "%A" (insert "hel" (insert "hello" emptyDict))

//Dictionary.lookup "DEKED" (Dictionary.empty () |> Dictionary.insert "DAD" |> Dictionary.insert "DEED" |> Dictionary.insert "DEIFLIED" |> Dictionary.insert "DEKED")

//Dictionary.lookup "DED" (Dictionary.empty () |> Dictionary.insert "DAD" |> Dictionary.insert "DEED")

open Gaddag

let emptyDict = empty ()

lookup "H>ello" (insert "Hello" emptyDict) |> printfn "%A"

0
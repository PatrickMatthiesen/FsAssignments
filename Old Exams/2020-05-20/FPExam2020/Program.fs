open System
open Exam2020

let testQ1() =
    (* Testsfor Q1.1 *)

(*
    printfn "%A" (insert true [])
    printfn "%A" (insert 5 [1; 3; 8; 9])
    printfn "%A" (insert 'c' ['a'; 'd'; 'e'])

    printfn "%A" (insertionSort [5; 3; 1; 8; 9])
    printfn "%A" (insertionSort ['o'; 'l'; 'H'; 'e'; 'l'])
*)

    ()

let testQ2() =
    // place debug prints for Q2 here
    ()

let testQ3 () =
    // place debug prints for Q3 here
    ()

let testQ4 () =
    // place debug prints for Q4 here
    //read >>= (fun s -> Option.defaultValue "nope" s |> write) |> evalSM |> ignore
    calculateRPN () |> ignore
    //read >>= (fun (Some a1) -> read >>= (fun (Some a2) -> write (string (int a1 + int a2)))) |> evalSM
    ()

[<EntryPoint>]
let main argv =
    testQ4 ()
    0 // return an integer exit code

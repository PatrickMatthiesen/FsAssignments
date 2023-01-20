(* Load this file into the interacive envorinment
   (select all and Alt-Enter in VS).
   
   Cut-and pasting these lines will typically not work unless you provide
   the entire path in the #load command. 

   Some IDEs may still complain about the path, place the full path here if that is the case.
*)

#load "JParsec.fs"
#load "Xam.fs"
open Exam2021_2;;

length2 (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))

[1..10] |> List.map (fun x -> (x, -(x + 1), -(x + 2))) |> fun eqs -> parQuadratic eqs 3 5


let r1 = mkRat 2 3 |> Option.get
let r2 = mkRat 3 4 |> Option.get
let r3 = mkRat 4 5 |> Option.get
let r4 = mkRat 5 6 |> Option.get
let r5 = mkRat 6 7 |> Option.get

plus r1 r2 |> Option.get |> ratToString
minus r1 r2 |> Option.get |> ratToString
minus r2 r2 |> Option.get |> ratToString
mult r1 r2 |> Option.get |> ratToString
div r1 r2 |> Option.get |> ratToString
div r1 (minus r2 r2 |> Option.get)

r1 |> evalSM (smPlus r2) |> Option.get |> snd |> ratToString
r1 |> evalSM (smMinus r2) |> Option.get |> snd |> ratToString
r1 |> evalSM (smMult r2) |> Option.get |> snd |> ratToString
r1 |> evalSM (smDiv r2) |> Option.get |> snd |> ratToString



evalSM (calculate [(r2, smPlus); (r3, smMinus); (r4, smMult); (r5, smDiv)]) r1 |> Option.get |> snd |> ratToString
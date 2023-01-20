(* Load this file into the interacive envorinment
   (select all and Alt-Enter in VS).
   
   Cut-and pasting these lines will typically not work unless you provide
   the entire path in the #load command. 

   Some IDEs may still complain about the path, place the full path here if that is the case.
*)

#load "Exam.fs"
open Exam2020_2;;

let (hd, tl) = step llzero
let (hd1, tl1) = step tl

let (hd_2, tl_2) = step (cons 42 llzero)
let (hd1_2, tl1_2) = step tl_2

let (hdlst, tl_3) = init id |> takeFirst 10
let (hd1_3, tl1_3) = step tl_3

let (hd_4, tl_4) = step (unfold (fun st -> (st, st + 5)) 0);;
let (hd1_4, tl1_4) = step tl_4
let (hd2_4, tl2_4) = step tl1_4
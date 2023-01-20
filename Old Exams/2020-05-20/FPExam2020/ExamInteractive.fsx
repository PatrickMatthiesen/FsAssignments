(* Load this file into the interacive envorinment
   (select all and Alt-Enter in VS).
   
   Cut-and pasting these lines will typically not work unless you provide
   the entire path in the #load command. 

   Some IDEs may still complain about the path, place the full path here if that is the case.
*)

#load "Exam.fs"
open Exam2020

let rock : strategy = fun _ -> Rock
let paper : strategy = fun _ -> Paper
let scissors : strategy = fun _ -> Scissors

playTournament 5 [for i in 1..1000 do yield rock; yield paper; yield scissors]

calculateRPN ()

insertionSortTail [9;5;3;7]

let playTournament2 rounds players =
    let rec initRound acc =
        function 
        | [] -> (acc, [])
        | [x] -> (acc, [x])
        | x::y::xs -> initRound ((x,y):: acc) xs
        
    let rec aux =
        function
            | [] -> None
            | [(_,id)] -> Some id
            | players ->
                let (pairs, rest) = initRound [] players
                    
                pairs |> 
                List.map (fun ((p1,i1), (p2,i2)) -> async {
                    let (p1win,p2win) = bestOutOf p1 p2 |> Seq.item rounds
                    return 
                        if p1win = p2win
                        then None
                        else 
                            if p1win > p2win
                            then Some (p1,i1)
                            else Some (p2,i2)
                }) |>
                Async.Parallel |>
                Async.RunSynchronously |>
                List.ofArray |>
                List.filter Option.isSome |>
                List.map Option.get |>
                (fun lst -> aux (lst @ rest))
                    
    aux (List.mapi (fun i s -> (s,i)) players)

playTournament2 5 [for i in 1..1000 do yield rock; yield paper; yield scissors]
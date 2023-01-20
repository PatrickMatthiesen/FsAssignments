module Exam2021_2
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2021_2 = 
 *)

(* 1: Binary lists *)

(* Question 1.1 *)

    type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

    let rec length bl = 
        match bl with
        | Nil -> 0
        | Cons1 (_,b) -> length b + 1  
        | Cons2 (_,a) -> length a + 1  
    
(* Question 1.2 *)
    let split lst =
        let rec aux lst lstA lstB =
            match lst with
            | Nil -> (List.rev lstA, List.rev lstB)
            | Cons1 (a,b) -> aux b (a :: lstA) lstB 
            | Cons2 (b,a) -> aux a lstA (b :: lstB)
        aux lst List.empty List.empty

    let length2 lst =
        let rec aux lst countA countB =
            match lst with
            | Nil -> (countA, countB)
            | Cons1 (_,b) -> aux b (countA + 1) countB 
            | Cons2 (_,a) -> aux a countA (countB + 1)
        aux lst 0 0
        

(* Question 1.3 *)


    let rec map f g lst = 
        match lst with
        | Nil -> Nil
        | Cons1 (a,b) -> Cons1 (f a, (map f g b))
        | Cons2 (b,a) -> Cons2 (g b, (map f g a))

(* Question 1.4 *)

    let rec filter f g lst = 
        match lst with
        | Nil -> Nil
        | Cons1 (a,lst) -> if f a then Cons1 (a, (filter f g lst)) else filter f g lst
        | Cons2 (b,lst) -> if g b then  Cons2 (b, (filter f g lst)) else filter f g lst

(* Question 1.5 *)

    let rec fold f g acc =
        function
        | Nil -> acc
        | Cons1 (a, lst) -> fold f g (f acc a) lst
        | Cons2 (b, lst) -> fold f g (g acc b) lst   

(* 2: Code Comprehension *)
    let rec foo xs ys =
      match xs, ys with
      | [], ys -> ys
      | xs, [] -> xs
      | x :: xs, y :: ys when x < y ->
            x :: (foo xs (y :: ys))
      | x :: xs, y :: ys ->
            y :: (foo (x :: xs) ys)

    and bar =
      function
      | [] -> []
      | [x] -> [x]
      | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: foo : 'a list -> 'a list -> 'a list when 'a: comparison
       bar : 'a list -> 'a list when 'a: comparison


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: bar is a merge sort function, that uses foo for sorting
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: 
        foo = combineAndSort
        bar = mergeSort
    
    Q: What would be appropriate names of the values a and b in bar.
    
    
    A: left and right
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: you would use it for mutual recursion


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: it woyld work fine as foo does not use bar, 
        and bar comes after foo so it would still be able to use foo

    *)

(* Question 2.3 *) 
    let foo2 xs ys = 
        List.unfold (fun (xss,yss) ->
            match xss, yss with
            | [], [] -> None
            | [], y :: ys -> Some (y, ([], ys))
            | x :: xs, [] -> Some (x, (xs, []))
            | x :: xs, y :: ys when x < y ->
                Some (x, (xs, yss))
            | _, y :: ys ->
                Some (y, (xss, ys))
        ) (xs, ys)
    
    (* use the following code as a starting template
    let foo2 xs ys = List.unfold <a function goes here> (xs, ys)
    *)

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: <Your answer goes here>

    *)
(* Question 2.5 *)

    let fooTail a b = 
        let rec aux xss yss c = 
            match xss, yss with
            | [], yss -> c yss
            | xss, [] -> c xss
            | x :: xs, y :: ys when x < y ->
                aux xs yss (fun acc -> c (x :: acc))
            | _, y :: ys ->
                aux xss ys (fun acc -> c (y :: acc))
        aux a b id

(* Question 2.6 *)

    let barTail lst = 
        let rec aux lst c =
            match lst with
            | [] -> c []
            | [x] -> c [x]
            | xs ->
                let (a, b) = List.splitAt (List.length xs / 2) xs
                aux a (fun r1 -> 
                    aux b (fun r2 -> 
                    c (foo r1 r2)))
        aux lst id

(* 3: Approximating square roots *)

(* Question 3.1 *)

    open System

    let approxSquare (x: int) num = 
        let x = float x
        let rec nearestSQRT x (dis: float) =
            match sqrt (x + dis) % 1. = 0. with
            | true -> (x + dis)
            | false -> 
                nearestSQRT x (
                    match -dis with
                    | y when y <= 0. -> y-1.
                    | y -> y)

        let rec aux sqr num =
            if num > 0
            then aux (((x/sqr) + sqr)/2.0) (num-1)
            else sqr
            
        aux (nearestSQRT x 0. |> sqrt) num

(* Question 3.2 *)

    let quadratic (a: int) (b: int) (c: int) num =
        let aux op =
            let revB = float -b
            let sqr = approxSquare ((b * b) - (4 * a * c)) num
            let bottom = 2.0 * float a
            (op revB sqr) / bottom
        (aux (+), aux (-))


(* Question 3.3 *)

    let parQuadratic lst processes num =
        let aux lst = 
            List.map (fun (a,b,c) -> async { return quadratic a b c num }) lst |>
            Async.Parallel |>
            Async.RunSynchronously |>
            List.ofArray

        List.chunkBySize processes lst |>
        List.map aux |> List.collect id
        //List.rev |>
        //List.fold (fun acc ls -> ls@acc) []
        

(* Question 3.4 *)

    open JParsec.TextParser

    let solveQuadratic s num =
        let spacces = many (pchar ' ')
        let s = s + "#"

        pint32 .>> pstring "x^2" .>> spacces .>>.
        anyChar .>> spacces .>>.
        pint32 .>> pstring "x" .>> spacces .>>.
        anyChar .>> spacces .>>.
        pint32  .>> spacces .>>
        pchar '=' .>> spacces .>>
        pchar '0' .>> pchar '#' |>
        run <| s |> getSuccess |>
        (fun ((((a, op1),b),op2),c) -> 
            quadratic a (if op1 = '+' then b else -b) (if op2 = '+' then c else -c) num)


        

(* 4: Rational numbers *)

(* Question 4.1 *)

    type rat = int * int

(* Question 4.2 *)

    let mkRat a b = 
        let rec gcd x y =
            if y = 0 then x
            else gcd y (x % y)
        let g = gcd a b |> abs
        match a/g,b/g with
        | _, 0 -> None
        | a, b when b < 0 -> 
            Some (rat (-a, -b))
        | a, b -> Some (rat (a, b))

    let ratToString (a,b) = sprintf "%i / %i" a b

(* Question 4.3 *)

    let plus (a,b) (c,d) = mkRat ((a*d) + (b*c)) (b*d)
    let minus (a,b) (c,d) = mkRat ((a*d) - (b*c)) (b*d)
    let mult (a,b) (c,d) = mkRat (a*c) (b*d)
    let div (a,b) (c,d) = mkRat (a*d) (b*c)

(* Question 4.4 *)

    type SM<'a> = SM of (rat -> ('a * rat) option)
    let ret x = SM (fun st -> Some (x, st))
    let bind (SM m) f =
        SM (fun st ->
            match m st with
            | None -> None
            | Some (x, st') ->
                let (SM g) = f x
                g st')
        
    let (>>=) m f = bind m f
    let (>>>=) m n = m >>= (fun () -> n)
    let evalSM (SM f) s = f s 

    let evalRat option = Option.map (fun o -> ((), o)) option;;

    let smPlus rat = SM (fun st -> plus rat st |> Option.map (fun o -> ((), o) ))

    //let smPlus rat = SM (fun s -> plus rat s |> evalRat)
    let smMinus rat = SM (fun s -> minus s rat |> evalRat)
    let smMult rat = SM (fun s -> mult rat s |> evalRat)
    let smDiv rat = SM (fun s -> div s rat |> evalRat)

(* Question 4.5 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let rec calculate lst = 
        state {
            match lst with
            | [] -> return ()
            | (rat, op) :: xs ->
                do! op rat
                return! calculate xs
        }

    (+) 5 6
module Exam2020

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020 = *)

(* 1: Insertion sort *)

(* Question 1.1 *)

    let rec insert x lst = 
        match lst with
        | [] -> [x]
        | y :: ys -> 
            if x <= y 
            then x :: lst 
            else y :: insert x ys
    
    let rec insertionSort lst = 
        match lst with
        | [] -> []
        | x :: xs -> insert x (insertionSort xs)
        
    
(* Question 1.2 *)

    let insertTail x lst = 
        let rec aux lst acc =
            match lst with
            | [] -> List.rev (x::acc)
            | y :: ys -> 
                if x <= y 
                then List.rev acc @ x :: lst
                else aux ys (y :: acc)
            
        aux lst []
    
    let insertionSortTail lst = 
        let rec aux lst acc = 
            match lst with
            | [] -> acc
            | x :: xs -> aux xs (insertTail x acc)

        aux lst []



(* Question 1.3 *)

    (* 
    Q: Why are the higher-order functions from the List library 
    not a good fit to implement insert?

    A: <Your answer goes here>
    you would have to go through the entire list every time as you cant break out of a fold in the middle of a list, wich makes the run time quite high
    *)

    let insertionSort2 lst =
        List.foldBack insertTail lst []

(* Question 1.4 *)

    let insertBy f e lst = 
        let rec aux x lst acc =
            match lst with
            | [] -> e :: acc
            | y :: ys -> 
                if f x <= f y 
                then x :: lst 
                else y :: aux x ys acc
        aux e lst []

    let insertionSortBy f lst =
        let rec aux lst acc = 
            match lst with
            | [] -> acc
            | x :: xs -> aux xs (insertBy f x acc)

        aux lst []

(* 2: Code Comprehension *)
    let rec foo x = 
        function 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

    let rec bar x =
        function
        | []        -> []
        | xs :: xss -> (x :: xs) :: bar x xss 

    let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: <Your answer goes here>
        foo : 'a -> 'a list -> 'a list
        bar : 'a -> 'a list list -> 'a list list
        baz : 'a list -> 'a list list

    Q: What do functions foo, bar, and baz do? 
       Focus on what they do rather than how they do it.

    A: <Your answer goes here>
        foo removes the first instance of the given item from the list
        bar adds an element to all the lists in the list
        baz makes the permutations of the input list

    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: <Your answer goes here>
        foo = remove
        bar = addAll
        baz = permutations
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo generates a warning during compilation: 
    Warning: Incomplete pattern matches on this expression.

    
    Q: Why does this happen, and where? 

    A: <Your answer goes here>
        the empty list is not matched

    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: <Your answer goes here>
        no it wouldn't cause problems, as the result of baz is never empty when foo is called
    *)

    let rec foo2 x = 
        function 
        | [] -> []
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

(* Question 2.3 *) 

    (* 
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: 'a list -> 'a list list

    Q: What does it do? Focus on what it does rather than how it does it.

    A: it makes the permutations of the input list, without the first instance of the element y
        
    *)

(* Question 2.4 *)

    let bar2 x lst = 
        List.map (fun el -> x :: el) lst

(* Question 2.5 *)

    let rec baz2 = 
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            List.foldBack (fun y acc -> ((foo2 y >> baz2 >> bar2 y) xs) @ acc) xs []


(* Question 2.6 *)

    (*
    
    Q: The function foo is not tail recursive. Why?
    
    A: it has no acumulator nor continuation function, so every recursive call to foo waits for the return to do the append (::)

    *)

    let fooTail x =
        let rec aux acc =
            function
            | [] -> []
            | y :: ys when x = y -> List.rev acc @ ys
            | y :: ys -> aux (y::acc) ys
        aux []

(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

    type shape = Rock | Scissors | Paper
    let mkShape (s: string) = 
        match s.ToLower () with
        | "rock" -> Rock
        | "scissors" -> Scissors
        | "paper" -> Paper
        | s       -> failwith (sprintf "invalid shape: %s" s) 
        

    let shapeToString =
       function
       | Rock     -> "rock"
       | Paper    -> "paper"
       | Scissors -> "scissors"

    type result = One | Two | Draw

    let resultToString =
        function
        | One -> "playerOneWin"
        | Two -> "playerTwoWin"
        | Draw -> "draw"


    let rps s1 s2 = 
        match s1, s2 with
        | Rock, Scissors -> One
        | Scissors, Paper -> One
        | Paper, Rock -> One

        | Scissors, Rock -> Two
        | Paper, Scissors -> Two
        | Rock, Paper -> Two

        | _,_ -> Draw

(* Question 3.2 *)

    type strategy = (shape * shape) list -> shape

    let parrot s : strategy =
        fun moves -> 
            match moves with
            | (_,move) :: _ -> move
            | [] -> s
            
    
    let beatingStrat moves =
        let list = 
            List.countBy (fun (_,m) -> m) moves |> 
            List.sortBy (fun (_,c) -> c) |> 
            List.fold (fun acc (m,c) ->
                match acc with
                | [] -> [(m,c)]
                | (_,prevC) :: _ -> 
                    if prevC > c then acc 
                    else (if prevC = c then (m,c) :: acc else [(m,c)])
            ) []

        List.foldBack (fun m acc -> 
            List.fold (fun acc (s,_) -> if rps m s = One then m else acc) acc list
        ) [Rock; Paper; Scissors] Rock
            

    let roundRobin (shapes: shape list) : strategy = 
        let mutable count = 0
        fun moves ->
            let oldCount = count
            count <- (count+1)%shapes.Length
            shapes.[oldCount]
        

(* Question 3.3 *)

    (* 
    
    Q: It may be tempting to generate a function that calculates your 
       point tuple after n rounds and then use Seq.initInfinite to 
       generate the sequence. This is not a good solution. Why?

    A: <Your answer goes here>
    
    *)

    let bestOutOf (s1: strategy) (s2: strategy) = 
        let update (p1, p2) res = 
            match res with
            | One -> (p1+1,p2)
            | Two -> (p1,p2+1)
            | Draw -> (p1,p2)
        
        let aux (p1,p2) m1 m2 =
            let move1 = s1 m1
            let move2 = s2 m2
            (update (p1,p2) (rps move1 move2), (move1, move2) :: m1, (move2, move1) :: m2)

        Seq.unfold (fun (result, m1, m2) -> Some (result, aux result m1 m2)) ((0,0), [], [])

(* Question 3.4 *)

    let playTournament rounds (strats: strategy list) = 
        let rec pairAndPlay strats acc =
            match strats with
            | [] -> (acc, [])
            | [x] -> (acc, [x])
            | (i1,s1) :: (i2,s2) :: xs -> 
                pairAndPlay xs (
                async {
                    let (p1, p2) = bestOutOf s1 s2 |> Seq.item (rounds)
                    return 
                        if p1 = p2
                        then None
                        else 
                            if p1 > p2
                            then Some (i1,s1)
                            else Some (i2,s2)
                } 
                :: acc
                )

        let rec aux strats = 
            match strats with
            | [] -> None
            | [(id, _)] -> Some id
            | _ ->
                let (pairs, rest) = pairAndPlay strats [] 
                
                pairs |>
                Async.Parallel |>
                Async.RunSynchronously |>
                List.ofArray |>
                List.fold (fun acc o -> 
                    match o with
                    | Some a -> a :: acc
                    | None -> acc
                ) [] |> List.rev |>
                (fun lst -> aux (lst @ rest))
        
        aux (List.mapi (fun i s -> (i,s)) strats)


(* 4: Revers Polish Notation *)

(* Question 4.1 *)

    type stack = int list

    let emptyStack = []

(* Question 4.2 *)

    type SM<'a> = S of (stack -> ('a * stack) option)

    let ret x = S (fun s -> Some (x, s))
    let fail  = S (fun _ -> None)
    let bind f (S a) : SM<'b> = 
        S (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (S g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (S f) = f emptyStack 

    let push i =
        S (fun s -> Some ((), i :: s))

    let pop = 
        S (fun s ->
            match s with
            | [] -> None
            | x :: xs -> Some (x, xs)
            
        )

(* Question 4.3 *)

    let write str : SM<unit> =  S (fun s -> printf "%s" str; Some ((), s)) //ret (printf "%s" str)

    let read =
        let rec aux acc =
            match System.Console.Read() |> char with
            | '\n' when acc = [] -> None
            | c    when System.Char.IsWhiteSpace c -> 
                acc |> List.fold (fun strAcc ch -> (string ch) + strAcc) "" |> Some
            | c -> aux (c :: acc)

        S (fun s -> Some (aux [], s))

    (* 
    
    Q: Consider the definition of write There is a reason that the definition 
       is S (fun s -> printf "%s" str; Some ((), s)) and not just 
       ret (printf "%s" str). For a similar reason, in read, we write 
       S (fun s -> Some (aux [], s)) and not ret (aux []). 
       What is the problem with using ret in both of these cases?
    
    A: <Your answer goes here>
        timeing, we want the calls to happen when we write them not when the function is made
    *)

(* Question 4.4 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()
    
    let isInt (str : string) : bool = System.Int32.TryParse str |> fst
    
    let binop op = 
        state {
            let! x = pop
            let! y = pop
            return! push (op x y)
        }

    let calculateRPN () =
        let handle s =
            match s with 
            | "+" -> binop (+)
            | "-" -> binop (-)
            | "*" -> binop (*)
            | _ when isInt s -> push (System.Int32.Parse s)
            | _ -> fail
        
        let rec aux () =
            state {
                let! str = read
                match str with
                | Some s -> 
                    do! handle s
                    return! aux ()
                | None -> 
                    let! r = pop
                    return! write (string r)
            }
        
        aux () |> evalSM
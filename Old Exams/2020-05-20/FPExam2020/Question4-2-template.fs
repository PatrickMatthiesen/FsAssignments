(* 4: Revers Polish Notation *)
module m4
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

let write str : SM<unit> = S (fun s -> printf "%s" str; Some ((), s))

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

    A: <Your answen goes here>

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

let calculateRPN () =
        let handle s =
            state {        
            match s with 
            | "+" -> 
                let! x = pop
                let! y = pop
                return! push (x+y)
            | "-" ->
                let! x = pop
                let! y = pop
                return! push (y-x)
            | "*" ->
                let! x = pop
                let! y = pop
                return! push (x*y)
            | _ ->
                if isInt s then
                    return! push (System.Int32.Parse s)
                else
                    return! fail
            }
        
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
        
        aux ()

//[<EntryPoint>]
let main argv =
    calculateRPN () |> evalSM |> 
    function 
    | None -> printfn "Invalid formula"
    | _    -> ()
    0 // return an integer exit code

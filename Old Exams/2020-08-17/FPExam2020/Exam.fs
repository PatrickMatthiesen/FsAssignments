module Exam2020_2
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
(* module Exam2020_2 = *)

(* 1: Binary search trees *)

    type 'a bintree = 
    | Leaf
    | Node of 'a bintree * 'a * 'a bintree

(* Question 1.1 *)

    let rec insert x tree = 
        match tree with
        | Leaf -> Node (Leaf, x, Leaf)
        | Node (l, y, r) -> if x <= y then Node (insert x l, y, r) else Node (l, y, insert x r)
    
(* Question 1.2 *)

    let fromList lst = 
        let rec aux lst acc =
            match lst with
            | [] -> acc
            | x :: xs -> aux xs (insert x acc)
        aux lst Leaf

(* Question 1.3 *)

    let fold f acc tree = 
        let rec aux tree acc =
            match tree with
            | Leaf -> acc
            | Node (l, y, r) -> f (aux l acc) y |> aux r
        aux tree acc
    
    let foldBack f acc tree = 
        let rec aux tree acc =
            match tree with
            | Leaf -> acc
            | Node (l, y, r) -> f (aux r acc) y |> aux l
        aux tree acc

    let inOrder tree = foldBack (fun acc x -> x :: acc) [] tree


(* Question 1.4 *)

    (* 

    Q: Consider the following map function

    *)

    let rec badMap f =
      function
      | Leaf -> Leaf
      | Node (l, y, r) -> Node (badMap f l, f y, badMap f r)

    (*
    Even though the type of this function is `('a -> 'b) -> 'a bintree -> 'b bintree` 
    as we would expect from a map function, this  function does not do what
    we want it to do. What is the problem? Provide an example to demonstrate the problem.

    A: i guess it could mess with the order of the items in case the given function makes everything negative, 
        because then the order of the items wouldn't be correct anymore. so it should apply the funktion and then make a new tree
       badMap is also not tailrecursive and this it could be bad having so many things on the stack at the same time
    *)

    let rec map f tree = fold (fun acc x -> insert (f x) acc) Leaf tree

(* 2: Code Comprehension *)
    let rec foo =
        function 
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs

    let rec bar =
        function
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    let rec baz =
        function
        | []               -> []
        | lst when bar lst -> lst
        | lst              -> baz (foo lst)
     

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: foo : 'a list -> 'a list when 'a: comparison
       bar : 'a list -> bool when 'a: comparison
       baz : 'a list -> 'a list when 'a: comparison


    Q: What do functions ```bar```, and ```baz``` do 
       (not `foo`, we admit that it is a bit contrived)? 
       Focus on what they do rather than how they do it.

    A: bar checks if the list is sorted
       baz sorts the list (using selection sort?)


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo = partialSort or sortFirstElement
       bar = isSorted
       baz = sort (or selectionSort?)
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The functions foo and bar generate a warning during compilation: 
    'Warning: Incomplete pattern matches on this expression.' 
    
    Q: Why does this happen, and where? 

    A: foo and bar doesn't match the empty list


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: it shouldn't cause any problems as the lists never have less than one element when matched, as when two items are removed, then one of them is put back.

    *)

    let foo2 lst = 
        match lst with
        | []                  -> []
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs

    let bar2 lst = 
        match lst with
        | []          -> true
        | [x]         -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    (* Uncomment code to run after you have written foo2 and bar2 *)
    (*
    let rec baz2 =
      function
      | lst when bar2 lst -> lst
      | lst               -> baz2 (foo2 lst)
    *)

(* Question 2.3 *) 

    (* Consider this alternative definition of *)

    let rec foo3 =
      function 
      | [x]                 -> [x]
      | x::xs               -> x :: foo3 xs
      | x::y::xs when x > y -> y :: (foo3 (x::xs))

    (*

    Q: Do the functions `foo` and `foo3` produce the same output for all possible inputs? 
       If yes, why; if no why not and provide a counter example.

    A: no they dont, the third match is never matched, as the second match always will be hit if there is more than one element in the list

    *)

(* Question 2.4 *)

    let bar3 lst =
        match lst with
        | [] -> true
        | x :: xs ->         
            List.fold (fun (isSorted, x) y -> 
                if isSorted 
                then (x <= y, y) 
                else (isSorted, y)
            ) (true, x) xs |> fst

(* Question 2.5 *)

    (*

    Q: The function foo or baz is not tail recursive. Which one and why?
    
    A: foo is not, and its because it has to wait for the recursive call to return, before it can append the ellement to the list

    *)

    (* ONLY implement the one that is NOT already tail recursive *)

    let fooTail lst = 
        let rec aux lst acc =
            match lst with
            | [] -> acc
            | x::(y::xs) when x > y -> aux (x::xs) (y::acc)
            | x::xs                       -> aux (xs) (x::acc)
        aux lst [] |> List.rev
        
    let bazTail _ = failwith "not implemented"

(* 3: Big Integers *)

(* Question 3.1 *)

    type bigInt = byte list //or just a sequence

    let fromString (s: string) = 
        [for c in s -> uint8 c - uint8 '0'] |> List.rev

    let toString : bigInt -> string = 
        List.rev >> List.map (fun i -> string i) >> String.concat ""

(* Question 3.2 *)

    let add x y =
        let auxAdd x y z acc =
            let r = x + y + z 
            (r/10uy, (r%10uy) :: acc)

        let rec aux xss yss (z, acc) =
            match xss, yss with
            | [],[] -> List.rev (if z > 0uy then z :: acc else acc)
            | [], y :: ys -> aux [] ys (auxAdd 0uy y z acc)
            | x :: xs, [] -> aux xs [] (auxAdd x 0uy z acc)
            | x :: xs, y :: ys -> aux xs ys (auxAdd x y z acc)

        aux x y (0uy, [])

(* Question 3.3 *)

    let multSingle x i = 
        let rec aux i acc =
            if i > 0
            then aux (i-1) (add x acc)
            else acc

        aux i (fromString "0")

(* Question 3.4 *)

    let mult x y = 
        let (x, y) = if List.length x > List.length y then (x,y) else (y,x)

        let rec aux yss pad acc =
            match yss with
            | [] -> acc
            | y :: ys -> aux ys (pad+1) (add acc ([for _ in 0..pad -> 0uy] @ multSingle x (int y)))

        aux y -1 (fromString "0")

(* Question 3.5 *)

    let fact x threads = 
        match x with
        | 0 -> [1uy]
        | _ -> 
             [for i in 1..x -> i |> string |> fromString] |> List.chunkBySize threads |>
             List.map (fun x -> async { return List.fold mult [1uy] x })
             |> Async.Parallel
             |> Async.RunSynchronously
             |> Array.fold mult [1uy]


(* 4: Lazy lists *)

    type 'a llist = Cons of (unit -> ('a * 'a llist))

    let rec llzero = Cons (fun () -> (0, llzero))

(* Question 4.1 *)

    let step (Cons ll) = 
        ll ()

    let cons x ll =
        Cons (fun () -> (x, ll))

(* Question 4.2 *)

    let init f = 
        let rec aux x = Cons (fun () -> (f x, aux (x+1)))
        aux 0

(* Question 4.3 *)

    let rec llmap f ll = 
        Cons (step ll |> fun (h,t) () -> (f h, llmap f t))

(* Question 4.4 *)

    let rec filter f ll = 
        let h, t = step ll
        match f h with
        | true -> Cons (fun () -> h, filter f t)
        | false -> filter f t

(* Question 4.5 *)

    let takeFirst x ll =
        let rec aux x ll acc =
            match x, step ll with
            | 0, _ -> (List.rev acc, ll)
            | _, (h,t) -> aux (x-1) t (h::acc)
        aux x ll []

(* Question 4.6 *)

    let rec unfold f state = 
        Cons (f state |> fun (h,t) () -> (h, unfold f t))

    (* Consider the following two implementations of Fibonacci sequences fibll1 and fibll2: *)

    let fib x =
        let rec aux acc1 acc2 =
            function
            | 0 -> acc1
            | x -> aux acc2 (acc1 + acc2) (x - 1)

        aux 0 1 x

    (* Uncomment after you have implemented init and unfold *)


    let fibll1 = init fib
    let fibll2 = unfold (fun (acc1, acc2) -> (acc1, (acc2, acc1 + acc2))) (0, 1)
    
    (* 

    Q: Both fibll1 and fibll2 correctly calculate a lazy list of Fibonacci numbers. 
       Which of these two lazy lists is the most efficient implementation and why?
    
    A: fibll2 is faster, as fibll1 has to recalculate all old values, while fibll2 is working on the previous numbers
    
    *)

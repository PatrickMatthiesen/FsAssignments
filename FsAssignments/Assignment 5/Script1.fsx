#time

5.1
let sum m n = 
    let rec aux acc n = //tail recursion
        match n with // du skal se om n er 0 for at stoppe
        | 0 -> acc + m // du ender med at skulle lave udregningen: alt der har været + m + 0
        | _ -> aux (acc + (m + n)) (n-1) // for at lave udregningen skal man tage acc og lægge den nye udregning til den, og gøre n en mindre
    aux 0 n // m er lige
sum 0 10

5.2
let length lst =
    let rec aux lst acc =
        match lst with
        | [] -> acc
        | x::xs -> aux xs (acc+1)
    aux lst 0
length [1;1;1;1]


5.3
let foldBack f lst acc =
    let rec aux lst c =
        match lst with
        | [] -> c acc
        | x::xs -> aux xs (fun r -> c (f x r))
    aux lst id

foldBack (-) [1..1000000] 0
foldBack (fun x acc -> acc@[x]) [1;2;3] []

5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x
factA 5

let factC x =
    let rec aux c x =
        match x with
        | 0 -> c 1
        | _ -> aux (fun r -> c (r*x)) (x-1)
    aux id x

factC 10000

let factC2 x =
    let rec aux f x acc =
        match x with
        | 0 -> acc
        | _ -> f x (aux f (x-1) acc)
    aux (fun x y -> x*y) x 1

factC2 5

5.5
let fibA x =
    let rec aux x acc1 acc2 =
        match x with
        | 0 -> acc1
        | _ -> aux (x-1) (acc1+acc2) acc1
    aux x 0 1

fibA 40

let fibC2 x =
    let rec aux x c =
        match x with 
        | 0 -> c 0
        | 1 -> c 1
        | _ -> aux (x-2) (fun r -> c r) + aux (x-1) (fun r -> c r)
    aux x id

let fibC x =
    let rec aux x c =
        match x with 
        | 0 -> c 0
        | 1 -> c 1
        | _ -> aux (x-2) (fun r1 -> 
               aux (x-1) (fun r2 -> 
               c (r1+r2)))
    aux x id

fibC 40

5.6
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

bigListK id 130000
//the :: makes the function non iterative, as it waits in the stack for the array to be combined

5.7
type word = (char * int) list

type aExp =
    | N of int (* Integer literal *)
    | V of string (* Variable reference *)
    | WL (* Word length *)
    | PV of aExp (* Point value lookup at word index *)
    | Add of aExp * aExp (* Addition *)
    | Sub of aExp * aExp (* Subtraction *)
    | Mul of aExp * aExp (* Multiplication *)
    | CharToInt of cExp (* NEW: Cast to integer *)
and cExp =
    | C of char (* Character literal *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp (* Convert character to upper case *)
    | ToLower of cExp (* Convert character to lower case *)
    | IntToChar of aExp (* NEW: Cast to character *)

open System
let rec arithEvalSimple a (w: word) (s: Map<string, int>) =
    match a with
    | N value -> value
    | V str  -> if s.ContainsKey str then s.[str] else 0
    | WL -> w.Length
    | PV x -> snd w.[arithEvalSimple x w s]
    | Add (a,b) -> arithEvalSimple a w s + arithEvalSimple b w s
    | Sub (a,b) -> arithEvalSimple a w s - arithEvalSimple b w s
    | Mul (a,b) -> arithEvalSimple a w s * arithEvalSimple b w s
    | CharToInt c -> int (charEvalSimple c w s) - int '0' 
and charEvalSimple c w s =
    match c with
    | C(c) -> c
    | ToUpper(c) -> Char.ToUpper (charEvalSimple c w s)
    | ToLower(c) -> Char.ToLower (charEvalSimple c w s)
    | CV(a) -> fst w.[arithEvalSimple a w s]
    | IntToChar a -> char (arithEvalSimple a w s + int '0')

let rec arithEvalTail a (w: word) (s: Map<string, int>) (con: int -> 'a) =
    match a with
    | N value -> con value
    | V str  -> s.TryFind str |> Option.defaultValue 0 |> con
    | WL -> con w.Length
    | PV x -> arithEvalTail x w s (fun r -> con (snd w.[r]))
    | Add (a,b) -> arithEvalTail a w s (fun x -> arithEvalTail b w s (fun y -> con (x+y))) 
    | Sub (a,b) -> arithEvalTail a w s (fun x -> arithEvalTail b w s (fun y -> con (x-y)))
    | Mul (a,b) -> arithEvalTail a w s (fun x -> arithEvalTail b w s (fun y -> con (x*y)))
    | CharToInt c -> charEvalTail c w s (fun r -> con (int r - int '0'))
and charEvalTail c w s (con: char -> 'a) =
    match c with
    | C(c) -> con c
    | ToUpper(c) -> charEvalTail c w s (fun r -> con (Char.ToUpper r))
    | ToLower(c) -> charEvalTail c w s (fun r -> con (Char.ToLower r))
    | CV(a) -> arithEvalTail a w s (fun r -> con (fst w.[r]))
    | IntToChar a -> arithEvalTail a w s (fun r -> con (char (r + int '0')))

let arithEval a w s = arithEvalTail a w s id
let charEval c w s = charEvalTail c w s id


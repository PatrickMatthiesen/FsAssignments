
// For more information see https://aka.ms/fsharp-console-apps
//printfn "Hello from F#"

//mark text and [alt + enter] to run

//1.1
let sqr x = x*x

//1.2
let pow x n = x**n


//1.3
let rec sum n =
    if n = 0 then 0 else n + (sum (n-1))

//sum 10

//1.4
let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2)

//1.5
let dup (s: string) = s + s

//1.6
let rec dupn s i = 
    if i > 0 
    then s+dupn (s) (i-1)
    else ""

//1.7
let rec bin (n, k) = 
    if n = k || k = 0 then 1
    else bin(n-1,k-1) + bin(n-1, k)


//1.8
let timediff (h,m) (h2,m2) = 
    (h2-h)*60+(m2-m)

//1.9
let minutes (h,m) = timediff (0,0) (h,m)

//1.10
let curry (f: 'a * 'b -> 'c) x y = f (x, y)

let uncurry (f: 'a -> 'b -> 'c) (x,y) = f x y

//printfn "%d" (timediff (12,34) (11,35))
//printfn "%d" (timediff (12,34) (13,35))

//printfn "%d" (minutes (14,24))
//printfn "%d" (minutes (23,1))

//printfn "%d" (curry (fun (x, y) -> x + y) 5 3)
//printfn "%d" (uncurry (fun x y -> x + y) (5, 3))

1.11
let empty (letter, value) = 
    fun pos -> (letter, value)
   
let theLetterA: int -> char * int = empty ('A', 1)

//theLetterA 0
//theLetterA 42
//theLetterA -762

1.12
let add newPos (letter, pointValue) word = 
    fun pos -> 
    if pos = newPos
    then (letter, pointValue)
    else word pos

//let add newPos (letter, pointValue) word = 
//    function
//    | pos when pos.Equals(newPos) -> (letter, pointValue)
//    | pos -> word pos


let theLettersAB = add 1 ('B', 3) theLetterA

printfn "%c %i" <|| theLettersAB 0

theLettersAB 0
theLettersAB 1


1.13
let hello: int -> char * int =
    empty ('H', 4) |>
    add 1 ('E', 1) |>
    add 2 ('L', 1) |>
    add 3 ('L', 1) |>
    add 4 ('O', 1)

//hello 0

1.14
let singleLetterScore word pos = snd (word pos)

let doubleLetterScore word pos = (singleLetterScore word pos) * 2

let trippleLetterScore word pos = (singleLetterScore word pos) * 3

//singleLetterScore hello 0

//doubleLetterScore hello 0


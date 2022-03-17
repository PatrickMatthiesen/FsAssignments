open System

2.1
let rec downto1 n = 
    if n > 0 
    then n :: (downto1 (n-1))
    else []

//downto1 2

let rec downto2 = 
    function
    | n when n > 0 -> n :: (downto2 (n-1))
    | _ -> []

//downto2 5

2.2
let removeOddIdx (xs: 'a list) = 
    [for i in 0..2..xs.Length-1 -> xs.[i]]

//removeOddIdx (downto2 10)
//removeOddIdx ([] : int list)
//removeOddIdx [true]
//removeOddIdx ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece"; "was"; "white"; "as"; "snow"]

2.3
let combinePair (xs: 'a list) = 
    [for i in 1..2..xs.Length-1 -> (xs.[i-1], xs.[i])]

//combinePair ([] : int list)
//combinePair [true; false]
//combinePair ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece"; "was"; "white"; "as"; "snow"]

2.4
type complex = float * float

let mkComplex x y : complex = (x,y)

let complexToPair (com: complex) = (fst com, snd com)

let inline (|+|) c1 c2 :complex = mkComplex (fst c1 + fst c2) (snd c1 + snd c2)
let inline (|*|) c1 c2 :complex = mkComplex ((fst c1 * fst c2) - (snd c1 * snd c2)) ((snd c1 * fst c2) + (fst c1 * snd c2))


//(mkComplex 1. 2.) |+| (mkComplex 1. 2.) 
//(mkComplex 1. 2.) |*| (mkComplex 1. 2.) 

let inline (|-|) (c1:complex) (c2:complex) = c1 |+| mkComplex (-(fst c2)) (-(snd c2))


let inline (|/|) c1 ((a2, b2): complex) :complex = c1 |*| mkComplex (a2 / (a2**2. + b2**2.)) (((-b2) / (a2**2. + b2**2.)))

((mkComplex (-3.3) 10.3) |/| (mkComplex (-3.2) (-2.0)))
//(-0.7050561798, -2.778089888)

2.5
let explode1 (s:string) = [for c in s -> c]

//explode1 "Hello World!"

let rec explode2 (s: string) = 
    match s with
    | s when s.Length > 0 -> s.[0] :: explode2 s.[1..]
    | _ -> []

//explode2 "Hello World!"


2.6
let implode (list: char list) = 
    String.Concat list

let implode2 (list: char list) :string = 
    List.fold (fun s c2 -> s + c2.ToString() ) "" list

let implodeRev (list: char list) :string = 
    List.fold (fun s c2 -> c2.ToString() + s ) "" list

//implodeRev ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!']

//implode2 ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!']

2.7
let toUpper (s:string) = s.ToUpper()
    //explode1 s |> List.map (fun c -> Char.ToUpper c) |> implode1

//toUpper ""
//toUpper "fuck this"

//List.map (fun c -> c.) "hi".ToUpper

2.8
let rec ack (m,n) = 
    match m with
    | 0 -> n+1
    | m when m> 0 && n=0 -> ack (m-1, 1)
    | m when m> 0 && n > 0 -> ack (m-1, ack (m,n-1))
    | _ -> 0

2.9
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start) 

//time (fun () -> ack (3, 11))


let timeArg1 f a = (fun () -> f a) |> time

//timeArg1 ack (3, 11)

2.10
let rec downto3 (f: int -> 'a -> 'a) (n: int) e = 
    if n > 0 
    then downto3 f (n-1) (f n e) 
    else e

let fac n = 
    downto3 (fun x acc -> x*acc) n 1

//fac 5

let range g n = [for i in 1..n -> g i]

//range (fun x -> x*2) 5

let range2 g n = downto3 (fun i acc -> (g i)::acc) n []

//range2 (fun i -> i+3) 5

//fac 0

range fac 10

2.11
type word = (char * int) list

let hello : word = [('H', 4); ('E',1);('L',1);('L',1);('O',1)]

2.12
type squareFun = word -> int -> int -> int

let singleLetterScore (w: word) pos acc = (snd w.[pos]) + acc
let doubleLetterScore (w: word) pos acc = (snd w.[pos]*2) + acc
let tripleLetterScore (w: word) pos acc = (snd w.[pos]*3) + acc

//singleLetterScore hello 4 0

2.13
let doubleWordScore (w: word) pos acc = acc*2
let tripleWordScore(w: word) pos acc = acc*3

//doubleWordScore hello 4 0
//doubleWordScore hello 12345 42

2.14
let isCons (c: char) = not ("aeiouyæøåAEIOUYÆØÅ ".Contains(c))

//let rec countConsonants (w: word) n = 
//    if (s.Length > 0) 
//    then    if isCons s.[0] 
//            then (countConsonants s.[1..] (n+1)) 
//            else (countConsonants s.[1..] n) 
//    else n

//countConsonants "hi im cool" 0

let oddConsonants (w: word) pos acc = 
    let mutable count = 0
    List.iter (fun (c,i) -> if isCons c then count <- count+1) w
    if count%2=1
    then -acc
    else acc

//let oddConsonantsFold (w: word) pos acc = 
//    let count = List.fold (fun n (c,i) -> if isCons c then n+1 else n) 0 w
//    if count%2=1
//    then -acc
//    else acc

//oddConsonants hello 1 5
//oddConsonantsFold hello 1 5


2.15

//type squareFun = word -> int -> int -> int
type square = (int * squareFun) list


//Use mapi on the square list
//map on the nested square , to create a list of type ((int * (int -> int)) list) list
//    where the priorities in squares have been left intact and the functions have
//    been partially applied with word and the correct index

let SLS : square = [(0, singleLetterScore)];;
let DLS : square = [(0, doubleLetterScore)];;
let TLS : square = [(0, tripleLetterScore)];;
let DWS : square = SLS @ [(1, doubleWordScore)];;
let TWS : square = SLS @ [(1, tripleWordScore)];;



//square list -> word -> int
let calculatePoints (list: ((int * (word -> int -> int -> int)) list) list) (word: word) :int = 
    List.mapi (fun i g -> List.map (fun (p,h) -> (p, (h word i))) g) list |>
    List.fold (fun acc item -> item@acc) [] |> //might be a type failure here, might be the pipe that dosnt give the correct thing
    List.sortBy (fun (p,_) -> p) |>
    List.map (fun (_,k) -> k) |>
    List.fold (fun acc m -> m acc) 0

//calculatePoints [DLS; SLS; TLS; SLS; DWS] hello
//calculatePoints [DLS; DWS; TLS; TWS; DWS] hello
    
    

//1.1
type direction = North | East | South | West
type coord = C of int * int


let move dist dir (C (x,y)) = 
    match dir with
    | North -> C (x, y-dist)
    | South -> C (x, y+dist)
    | East -> C (x+dist, y)
    | West -> C (x-dist, y)

move 10 North (C (0, 0))

let turnRight dir =
    match dir with
    | North -> East
    | South -> West
    | East -> South
    | West -> North


let turnLeft dir =
    match dir with
    | North -> West
    | South -> East
    | East -> North
    | West -> South

turnRight North
turnLeft North

//1.2
type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int

let step (P (coord, dir)) m =
    match m with
    | TurnLeft -> P (coord, turnLeft dir)
    | TurnRight -> P (coord, turnRight dir)
    | Forward dist -> P (move dist dir coord, dir)

step (P (C (0, 0), North)) TurnRight
step (P (C (0, 0), North)) TurnLeft
step (P (C (0, 0), North)) (Forward 10)

step (step (P (C (0, 0), North)) TurnRight) (Forward 10)

//1.3
let rec walk p ms = 
    match ms with
    | x :: xs -> walk (step p x) xs
    | [] -> p

walk (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

let walk2 p ms =
    List.fold (fun acc m -> step acc m) p ms

walk2 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

//1.4
let path (P (coord, dir)) ms = 
    let rec aux p ms =
        match ms with
        | x :: xs -> 
            match step p x, x with
            | P (coord, dir), Forward _ -> coord :: aux (P (coord, dir)) xs
            | pos, _ -> aux pos xs
        | [] -> []
    coord :: aux (P (coord, dir)) ms

path (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]
path (P (C (0, 0), North)) [Forward 5; TurnRight; Forward 5; TurnRight; Forward 5; TurnRight; Forward 5]

//1.5
let path2 (P (coord, dir)) ms = 
    let rec aux p ms acc =
        match ms with
        | x :: xs -> 
            match step p x, x with
            | P (coord, dir), Forward _ -> aux (P (coord, dir)) xs (coord :: acc)
            | pos, _ -> aux pos xs (acc)
        | [] -> acc

    aux (P (coord, dir)) ms [coord] |> List.rev

path2 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]

//1.6
//insert explanetion here

let path3 (P (coord,_) as pos) ms =
    let rec aux p ms c =
        match ms with
        | x :: xs -> 
            match step p x, x with
            | P (coord,_) as pos, Forward _ -> aux pos xs (fun a -> c (coord::a))
            | pos, _ -> aux pos xs c
        | [] -> c []

    aux pos ms (fun a -> coord :: a)

path3 (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]
path3 (P (C (0, 0), North)) [Forward 5; TurnRight; Forward 5; TurnRight; Forward 5; TurnRight; Forward 5]



//2.1
let foo f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y when Map.containsKey x m -> y
        | None ->
        m <- Map.add x (f x) m; f x
    aux

let rec bar x =
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)
and baz = foo bar



//foo takes a function that changes the next imput
//bar calles foo with bar

//the mutable map does nothing relevant
//if only the first line is removed then no it would not work, 
// but if i may assume that all things that have to do with the map is removed, then yes it would work and it would even be faster,
// because the map isn't used for anything at all, it only gets an item added but it never use it for anything

//bar could be called fib or fibonacci, foo could be called "call" or "run" and baz could be called "callFib"

//2.2
//the and keyword makes it posible for a function written before an other to call the later one, which otherwise wouldnt be posible as the compilation happens line by line
//if the and is made into a let, then it wouldn't be able to compile as bar wouldnt be able to call baz

//2.3
//the when makes the match more specific and thus it would need an other match that is just "Some y -> ..." or "_ -> ..."
//no it will never cause problems
let foo2 f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y -> y
        | None ->
        let r = f x
        m <- Map.add x r m; r
    aux


//2.4
#time

let rec barbaz x =
    let baz = foo barbaz
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

//i would assume barbaz is slower, as it has to make the function baz every time it recurses, and thus dont use the cach

//2.5
let bazSeq = 
    Seq.initInfinite baz

//Seq.item 6000 bazSeq

////baz 6000

//Seq.item 100100 bazSeq

//3.1
type element = string list

//3.2
let elToString (el: element) : string =
    Seq.map string el |> String.concat ""

let elFromString (s: string) : element =
    [for c in s -> string c]
    
    
//List.foldBack (fun (a, b) acc -> b.ToString() :: a :: acc) (Seq.countBy id el) []

//3.3
let nextElement (el: element) = 
    let rec aux (el: element) (acc: element) c =
        match el with
        | [] -> acc
        | x :: xs ->
            if x = c 
            then
                match acc with
                | [] -> aux xs [x] x
                | _ -> aux xs (x + acc.Head :: acc.Tail) x
            else aux xs (x :: acc) x

    aux el [] el.Head |> List.fold (fun acc a -> [for c in a.Length.ToString() -> string c] @ string a.[0] :: acc) []

//3.4
let elSeq el : element seq = 
    Seq.unfold (fun state -> Some (state, nextElement state)) el

let rec elSeq2 el : element seq = 
    seq { yield el
          yield! elSeq2 (nextElement el)  }

//"1" |> elFromString |> elSeq |> Seq.item 1

//"1" |> elFromString |> elSeq2 |> Seq.item 3

//3.5
#load "..\..\..\FsAssignments\Assignment 7\JParsec.fs"
//#load "JParsec.fs"

open JParsec.TextParser
open System

let elParse : Parser<element> =
     many (satisfy Char.IsDigit) .>> pchar '\n' |>> (fun s -> [for c in s -> string c]) <?> "couldnt parse string"

let elFromString2 s =
    run elParse (s+ "\n") |> getSuccess

elFromString2 "12345"


//4.1
type 'a ring = R of ('a list * 'a list)

//4.2
let length (R (a,b)) : int = (List.length a) + (List.length b)

let ringFromList (l: 'a list) : 'a ring = R ([],l)

let ringToList (R (a,b)) : 'a list = b @ (List.rev a)


//4.3
let empty : 'a ring = R ([],[])

let push el (R (a,b)) = R (a, el :: b)

let peek (R (a,b)) =
    match List.rev a, b with
    | x :: _, [] -> Some x
    | _, x :: _ -> Some x
    | [], [] -> None

let pop (R (a,b)) =
    match List.rev a, b with
    | _ :: xs, [] -> Some (R (List.rev xs, []))
    | _, _ :: xs -> Some (R (a, xs))
    | [], [] -> None

let cw (R (a,b)) =
    match a, List.rev b with
    | [], [] -> R (a,b)
    | [], x :: xs -> R (xs,[x])
    | x :: xs, _ -> R (xs,x::b)

let ccw (R (a,b)) =
    match List.rev a, b with
    | [], [] -> R (a,b)
    | x :: xs, [] -> R ([x],xs)
    | _, x :: xs -> R (x::a,xs)

[1;2;3] |> ringFromList |> ccw |> ccw |> ccw |> pop |> Option.get |> ringToList |> printfn "%A"

ringToList (ringFromList [1;2;3;4;5])
length (ringFromList [1;2;3;4;5])
ringToList (ringFromList [1;2;3;4;5])

//4.4
type StateMonad<'a, 'b> = SM of ('b ring -> ('a * 'b ring) option)

let ret x = SM (fun st -> Some (x, st))
let bind (SM m) f =
    SM (fun st ->
        match m st with
        | None -> None
        | Some (x, st') ->
            let (SM g) = f x
            g st'
    )

let (>>=) m f = bind m f
let (>>>=) m n = m >>= (fun () -> n)
let evalSM (SM f) s = f s

let fail = SM (fun _ -> None)

let smLength = SM (fun st -> Some (length st, st))

let smPush x = SM (fun st -> Some ((), push x st))

let smPop = 
    SM (fun st -> 
        match peek st with
        | None -> None
        | Some x -> Some (x, pop st |> Option.get)) 

let smCW = SM (fun st -> Some ((), cw st))

let smCCW = SM (fun st -> Some ((), ccw st))


type StateBuilder() =
    member this.Bind(x, f) = bind x f
    member this.Zero () = ret ()
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()

let ringStep =
    state {
        let! l = smLength
        if l > 1
        then
            let! x = smPop
            let! y = smPop
            if (x+y) % 2 = 1
            then 
                do! smPush y
                do! smPush x
                do! smCCW
    }

[1;2;2;4;5] |> ringFromList |> evalSM ringStep |> Option.get |> snd |> ringToList

[1;2;2;4;5] |> ringFromList |> evalSM (ringStep >>>= ringStep) |> Option.get |> snd |> ringToList

let rec iterRemoveSumEven (x: uint32) = 
    state {
        if x > 0u
        then 
            do! ringStep
            do! iterRemoveSumEven (x-1u)
    }
    
[1;2;3;4;5] |> ringFromList |> evalSM (iterRemoveSumEven 0u) |> Option.get |> snd |> ringToList

[1;2;3;4;5] |> ringFromList |> evalSM (iterRemoveSumEven 10u) |> Option.get |> snd |> ringToList

[1;2;3;4;5;6] |> ringFromList |> evalSM (iterRemoveSumEven 10u) |> Option.get |> snd |> ringToList


open System.Collections.Generic

let map = new Dictionary<char, int>()

let empty (letter: char, value: int) = 
    fun pos -> (letter, value)
   
let theLetterA: int -> char * int = empty ('A', 1)


1.1





let add newPos (letter, pointValue) word = 
    fun pos -> 
        match pos with
        | pointValue when pos.Equals(newPos) -> (letter, pointValue)
        | _ ->  word pos

let theLettersAB = add 1 ('B', 3) theLetterA

printfn "%c %i" <|| theLettersAB 1

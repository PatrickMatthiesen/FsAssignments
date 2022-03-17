type aExp =
| N of int // Integer value
| V of string // Variable
| WL // Length of the word
| PV of aExp // Point value of character at specific word index
| Add of aExp * aExp // Addition
| Sub of aExp * aExp // Subtraction
| Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let a1 = N 42
let a2 = N 4 .+. (N 5 .-. N 6)
let a3 = N 4 .*. N 2 .+. N 34
let a4 = (N 4 .+. N 2) .*. N 34
let a5 = N 4 .+. (N 2 .*. N 34)
let a6 = V "x"
let a7 = N 4 .+. (V "y" .-. V "z")

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

3.1
let rec arithEvalSimple (a: aExp) = 
    match a with
    | N(value) -> value
    | Add(a,b) -> arithEvalSimple a + arithEvalSimple b
    | Sub(a,b) -> arithEvalSimple a - arithEvalSimple b
    | Mul(a,b) -> arithEvalSimple a * arithEvalSimple b

//arithEvalSimple a1 //42
//arithEvalSimple a2 //3
//arithEvalSimple a3 //42
//arithEvalSimple a4 //204
//arithEvalSimple a5 //72


3.2
let rec arithEvalState a (s: Map<string, int>) = 
    match a with
    | N(value) -> value
    | V(str) -> if s.ContainsKey str then s.[str] else 0
    | Add(a,b) -> arithEvalState a s + arithEvalState b s
    | Sub(a,b) -> arithEvalState a s - arithEvalState b s
    | Mul(a,b) -> arithEvalState a s * arithEvalState b s

//arithEvalState a6 (Map.ofList [("x", 5)])
//arithEvalState a6 (Map.ofList [("y", 5)])
//arithEvalState a7 (Map.ofList [("x", 4); ("y", 5)])
//arithEvalState a7 (Map.ofList [("y", 4); ("z", 5)])

3.3
type word = (char * int) list
let hello : word = [('H', 4); ('E',1);('L',1);('L',1);('O',1)]

let rec arithEval a (w: word) (s: Map<string, int>) = // would be cleaner with: let calc b = arithEval b w s
    match a with
    | N value -> value
    | V str  -> if s.ContainsKey str then s.[str] else 0
    | WL -> w.Length
    | PV x -> snd w.[arithEval x w s]
    | Add (a,b) -> arithEval a w s + arithEval b w s
    | Sub (a,b) -> arithEval a w s - arithEval b w s
    | Mul (a,b) -> arithEval a w s * arithEval b w s

arithEval WL [] Map.empty
arithEval WL hello Map.empty
arithEval (PV (N 0)) hello Map.empty
arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)])
arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)])
arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)])
arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)])
arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)])
arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)])

open System
3.4
type cExp =
| C of char (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
| CV of aExp (* Character lookup at word index *)

let rec charEval c (w: word) (s: Map<string, int>) =
    match c with
    | C(c) -> c
    | ToUpper(c) -> Char.ToUpper (charEval c w s)
    | ToLower(c) -> Char.ToLower (charEval c w s)
    | CV(a) -> fst w.[arithEval a w s]

//charEval (C 'H') [] Map.empty
//charEval (ToLower (CV (N 0))) hello Map.empty
//charEval (ToUpper (C 'h')) [] Map.empty
//charEval (ToLower (C '*')) [] Map.empty
//charEval (CV (V "x" .-. N 1)) hello (Map.ofList [("x", 5)])


3.5
type bExp =
| TT (* true *)
| FF (* false *)

| AEq of aExp * aExp (* numeric equality *)
| ALt of aExp * aExp (* numeric less than *)

| Not of bExp (* boolean not *)
| Conj of bExp * bExp (* boolean conjunction *)

| IsDigit of cExp (* check for digit *)
| IsLetter of cExp (* check for letter *)
| IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let rec boolEval b (w: word) (s: Map<string, int>) = 
    match b with
    | TT -> true
    | FF -> false

    | AEq(a1,a2) -> arithEval a1 w s = arithEval a2 w s
    | ALt(a1,a2) -> arithEval a1 w s < arithEval a2 w s

    | Not(b1) -> not (boolEval b1 w s)
    | Conj(b1, b2) -> boolEval b1 w s && boolEval b2 w s

    | IsDigit(c) -> Char.IsDigit (charEval c w s)
    | IsLetter(c) -> Char.IsLetter (charEval c w s)
    | IsVowel(c) -> ("aeiouæøåAEIOUÆØÅ ".Contains(charEval c w s))

//boolEval TT [] Map.empty
//boolEval FF [] Map.empty
//boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [("x", 5); ("y", 7)])
//boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [("x", 5); ("y", 7)])
//boolEval (IsLetter (CV (V "x"))) hello (Map.ofList [("x", 4)])
//boolEval (IsLetter (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)])
//boolEval (IsDigit (CV (V "x"))) hello (Map.ofList [("x", 4)])
//boolEval (IsDigit (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)])

3.6
let isConsonant (c: cExp) = 
    Not (IsVowel c)

boolEval (isConsonant (C 'H')) [] Map.empty
boolEval (isConsonant (C 'h')) [] Map.empty
boolEval (isConsonant (C 'A')) [] Map.empty
boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 0)])
boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 1)])

3.7
type stmnt =
| Skip (* does nothing *)
| Ass of string * aExp (* variable assignment *)
| Seq of stmnt * stmnt (* sequential composition *)
| ITE of bExp * stmnt * stmnt (* if-then-else statement *)
| While of bExp * stmnt (* while statement *)

let rec evalStmnt stm (w: word) (s: Map<string, int>) = 
    match stm with
    | Skip -> s
    | Ass (str,a) -> s.Add (str, arithEval a w s)
    | Seq (stm1,stm2) -> evalStmnt stm1 w s |> evalStmnt stm2 w
    | ITE (b, stm1, stm2) -> if boolEval b w s then evalStmnt stm1 w s else evalStmnt stm2 w s
    | While (b,stm1) -> if boolEval b w s then evalStmnt stm1 w s |> evalStmnt (While (b,stm1)) w else s

evalStmnt Skip [] Map.empty
evalStmnt (Ass ("x", N 5)) [] Map.empty
evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty
evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty
evalStmnt (ITE (WL .<. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty
evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello Map.empty
evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello (Map.ofList [("x", 3); ("y", 100)])


3.8
type squareFun = word -> int -> int -> int

let stmntToSquareFun stm : squareFun = fun w pos acc -> (evalStmnt stm w (Map.ofList [("_pos_", pos); ("_acc_", acc)])).["_result_"]


let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))
let containsNumbers = stmntToSquareFun (Seq (Ass ("_result_", V "_acc_"), While (V "i" .<. WL, ITE (IsDigit (CV (V "i")),Seq (Ass ("_result_", V "_result_" .*. N -1), Ass ("i", WL)), Ass ("i", V "i" .+. N 1)))))

singleLetterScore hello 0 0
doubleLetterScore hello 0 0
tripleLetterScore hello 0 0
singleLetterScore hello 0 42
doubleLetterScore hello 0 42
tripleLetterScore hello 0 42
containsNumbers hello 5 50
containsNumbers (('0', 100)::hello) 5 50
containsNumbers (hello @ [('0', 100)]) 5 50


2.9
let oddConsonants = (Seq (Ass ("_result_", V "_acc_"), While (V "i" .<. WL, ITE (IsVowel (CV (V "i")), Ass ("i", V "i" .+. N 1), Seq (Ass ("_result_", V "_result_" .*. N -1), Ass ("i", V "i" .+. N 1))))))


//stmntToSquareFun (While (Not IsVowel ) )) w pos;;

stmntToSquareFun oddConsonants hello 1 5
stmntToSquareFun oddConsonants [('H', 4); ('H', 4)] 1 5
stmntToSquareFun oddConsonants [('H', 4); ('H', 4); ('O', 4)] 1 5


3.10
type square = (int * squareFun) list
type square2 = (int * stmnt) list

let calculatePoints (list: square list) (word: word) :int = 
    List.mapi (fun i g -> List.map (fun (p,h) -> (p, (h word i))) g) list |>
    List.fold (fun acc item -> item@acc) [] |> //might be a type failure here, might be the pipe that dosnt give the correct thing
    List.sortBy (fun (p,_) -> p) |>
    List.map (fun (_,k) -> k) |>
    List.fold (fun acc m -> m acc) 0

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints2 (list: square2 list) (word: word) :int = 
    calculatePoints (List.map (fun sq2 -> List.map (fun (p,stm) -> (p, stmntToSquareFun stm)) sq2) list) word

calculatePoints2 [DLS; SLS; TLS; SLS; DWS] hello
calculatePoints2 [DLS; DWS; TLS; TWS; DWS] hello
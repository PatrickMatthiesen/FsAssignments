// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#load "StateMonad.fs"
#load "Eval.fs"

open StateMonad
open Eval
    

    
printfn "Testing lookup:"
printfn "%A" (lookup "x" |> 
                evalSM state)
printfn "%A" (push >>>= push >>>= lookup "y" |>
                evalSM state)
printfn "%A" (push >>>= push >>>= push >>>= lookup "z" |> 
                evalSM state)
printfn ""

printfn "Testing pop:"
printfn "%A" (push >>>= pop >>>= lookup "x" |> 
                evalSM state)
printfn "%A" (pop >>>= push >>>= lookup "x" |> 
                evalSM state)
printfn ""

printfn "Testing update:"
printfn "%A" (update "x" 7 >>>= lookup "x" |> 
                evalSM state)
printfn "%A" (push >>>= update "x" 7 >>>= lookup "x" |>
                evalSM state)
printfn "%A" (push >>>= update "x" 7 >>>= pop >>>= lookup "x" |> 
                evalSM state)
printfn "%A" (pop >>>= update "x" 7 >>>= push >>>= lookup "x" |> 
                evalSM state)
printfn "%A" (lookup "x" >>= 
                (fun v1 -> lookup "y" >>= 
                            (fun v2 -> update "x" (v1 + v2))) >>>= 
                lookup "x" |> 
                evalSM state)
printfn "%A" (lookup "x" >>= 
                (fun v1 -> lookup "y" >>= 
                            (fun v2 -> update "x" (v1 + v2))) >>>= 
                lookup "y" |> 
                evalSM state)
printfn ""

printfn "Testing declare:"
printfn "%A" (declare "z" >>>= lookup "z" |> 
                evalSM state)
printfn "%A" (declare "z" >>>= update "z" 123 >>>= lookup "z" |> 
                evalSM state)
printfn "%A" (declare "x" >>>= lookup "x" |> 
                evalSM state)
printfn "%A" (declare "z" >>>= declare "z" |> 
                evalSM state)
printfn "%A" (declare "z" >>>= update "z" 123 >>>= push >>>= 
                declare "z" >>>= update "z" 456 >>>= lookup "z" |> 
                evalSM state)
printfn "%A" (declare "z" >>>= update "z" 123 >>>= push >>>= 
                declare "z" >>>= update "z" 456 >>>= pop >>>= 
                lookup "z" |> 
                evalSM state)
printfn "%A" (declare "_pos_" >>>= lookup "_pos_" |> 
                evalSM state)
printfn ""

printfn "Testing wordLength:"
printfn "%A" (wordLength |> 
                evalSM state)
printfn "%A" (wordLength |> 
                evalSM emptyState)
printfn ""

printfn "Testing characterValue:"
printfn "%A" (characterValue 0  |> 
                evalSM state)
printfn "%A" (characterValue 4  |> 
                evalSM state)
printfn "%A" (characterValue 8  |> 
                evalSM state)
printfn ""

printfn "Testing pointValue:"
printfn "%A" (pointValue 0  |> 
                evalSM state)
printfn "%A" (pointValue 3  |> 
                evalSM state)
printfn "%A" (pointValue 8  |> 
                evalSM state)
printfn ""

printfn "Testing add:"
printfn "%A" (add (ret 5) (ret 7)  |> 
                evalSM state)
printfn "%A" (add (lookup "x") (lookup "y")  |> 
                evalSM state)
printfn "%A" (add wordLength (lookup "z")  |> 
                evalSM state)
printfn ""


printfn "Testing div:"
printfn "%A" (div (ret 7) (ret 5)  |> 
                evalSM state)
printfn "%A" (div (lookup "y") (lookup "x")  |> 
                evalSM state)
printfn "%A" (div wordLength (lookup "z")  |> 
                evalSM state)
printfn "%A" (declare "z" >>>= div (lookup "x") (lookup "z")  |> 
                evalSM state)
printfn ""


printfn "Testing arithEval:"
printfn "%A" (arithEval (V "x" .+. N 10)  |> 
                evalSM state)
printfn "%A" (arithEval (WL .*. N 10)  |> 
                evalSM state)
printfn "%A" (arithEval (CharToInt (CV (N 0)))  |> 
                evalSM state)
printfn "%A" (arithEval (PV (N -5))  |> 
                evalSM state)
printfn "%A" (arithEval (V "x" .%. N 0)  |> 
                evalSM state)
printfn ""


printfn "Testing charEval:"
printfn "%A" (charEval (C 'H')  |> 
                evalSM state)
printfn "%A" (charEval (ToLower (CV (N 0)))  |> 
                evalSM state)
printfn "%A" (charEval (ToUpper (C 'h'))  |> 
                evalSM state)
printfn "%A" (charEval (CV (V "x" .-. N 1))  |> 
                evalSM state)
printfn ""


printfn "Testing boolEval:"
printfn "%A" (boolEval TT  |> 
                evalSM state)
printfn "%A" (boolEval FF  |> 
                evalSM state)
printfn "%A" (boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x"))  |> 
                evalSM state)
printfn "%A" (boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x"))  |> 
                evalSM state)
printfn "%A" (boolEval (IsVowel (CV (V "x")))  |> 
                evalSM state)
printfn "%A" (boolEval (IsLetter (CV (V "x" .-. N 1)))  |> 
                evalSM state)
printfn "%A" (boolEval (IsDigit (CV (N 1)))  |> 
                evalSM (mkState [] [('H', 0); ('1', 0)] []))
printfn "%A" (boolEval (IsDigit (CV (N 0)))  |> 
                evalSM (mkState [] [('H', 0); ('1', 0)] []))
printfn "%A" (boolEval (IsLetter (CV (N 1)))  |> 
                evalSM (mkState [] [('H', 0); ('1', 0)] []))
printfn "%A" (boolEval (IsLetter (CV (N 0)))  |> 
                evalSM (mkState [] [('H', 0); ('1', 0)] []))
printfn "%A" (boolEval (IsVowel (CV (V "x" .-. N 1)))  |> 
                evalSM state)

printfn ""


printfn "Testing stmntEval:"
printfn "%A" (stmntEval (Ass ("x", N 5)) >>>= lookup "x"  |> 
                evalSM emptyState)
printfn "%A" (stmntEval (Seq (Declare "x", Ass ("x", N 5))) >>>= lookup "x"  |> 
                evalSM emptyState)
printfn "%A" (stmntEval 
                    (Seq (Declare "x", 
                    Seq (Declare "y", 
                            Seq (Ass ("x", WL), 
                                Ass ("y", N 7))))) >>>= 
                lookup "x" >>= (fun vx -> lookup "y" >>= (fun vy -> ret (vx, vy)))  |> 
                evalSM emptyState)
printfn "%A" (stmntEval 
                    (Seq (Declare "x", 
                        Seq (Declare "y", 
                                Seq (Ass ("x", WL), 
                                    Ass ("y", N 7))))) >>>= 
                lookup "x" >>= (fun vx -> lookup "y" >>= (fun vy -> ret (vx, vy))) |> 
                evalSM state)
printfn ""

printfn "Testing arithEval2:"
printfn "%A" (arithEval2 (V "x" .+. N 10)  |> 
                evalSM state)
printfn "%A" (arithEval2 (WL .*. N 10)  |> 
                evalSM state)
printfn "%A" (arithEval2 (CharToInt (CV (N 0)))  |> 
                evalSM state)
printfn "%A" (arithEval2 (PV (N -5))  |> 
                evalSM state)
printfn "%A" (arithEval2 (V "x" .%. N 0)  |> 
                evalSM state)
printfn ""


printfn "Testing charEval2:"
printfn "%A" (charEval2 (C 'H')  |> 
                evalSM state)
printfn "%A" (charEval2 (ToLower (CV (N 0)))  |> 
                evalSM state)
printfn "%A" (charEval2 (ToUpper (C 'h'))  |> 
                evalSM state)
printfn "%A" (charEval2 (CV (V "x" .-. N 1))  |> 
                evalSM state)
printfn ""


printfn "Testing boolEval2:"
printfn "%A" (boolEval2 TT  |> 
                evalSM state)
printfn "%A" (boolEval2 FF  |> 
                evalSM state)
printfn "%A" (boolEval2 ((V "x" .+. V "y") .=. (V "y" .+. V "x"))  |> 
                evalSM state)
printfn "%A" (boolEval2 ((V "x" .+. V "y") .=. (V "y" .-. V "x"))  |> 
                evalSM state)
printfn "%A" (boolEval2 (IsVowel (CV (V "x")))  |> 
                evalSM state)
printfn "%A" (boolEval2 (IsLetter (CV (V "x" .-. N 1)))  |> 
                evalSM state)
printfn "%A" (boolEval2 (IsVowel (CV (V "x" .-. N 1)))  |> 
                evalSM state)

printfn ""


printfn "Testing stmntEval2:"
printfn "%A" (stmntEval2 (Ass ("x", N 5)) >>>= lookup "x"  |> 
                evalSM emptyState)
printfn "%A" (stmntEval2 (Seq (Declare "x", Ass ("x", N 5))) >>>= lookup "x"  |> 
                evalSM emptyState)
printfn "%A" (stmntEval2 
                    (Seq (Declare "x", 
                    Seq (Declare "y", 
                            Seq (Ass ("x", WL), 
                                Ass ("y", N 7))))) >>>= 
                lookup "x" >>= (fun vx -> lookup "y" >>= (fun vy -> ret (vx, vy)))  |> 
                evalSM emptyState)
printfn "%A" (stmntEval2 
                    (Seq (Declare "x", 
                        Seq (Declare "y", 
                                Seq (Ass ("x", WL), 
                                    Ass ("y", N 7))))) >>>= 
                lookup "x" >>= (fun vx -> lookup "y" >>= (fun vy -> ret (vx, vy))) |> 
                evalSM state)
printfn ""

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

let stmntSingleLetterScore = Ass ("_result_", arithSingleLetterScore)
let stmntDoubleLetterScore = Ass ("_result_", arithDoubleLetterScore)
let stmntTripleLetterScore = Ass ("_result_", arithTripleLetterScore)

let stmntDoubleWordScore = Ass ("_result_", arithDoubleWordScore)
let stmntTripleWordScore = Ass ("_result_", arithTripleWordScore)

let singleLetterScore = stmntToSquareFun stmntSingleLetterScore
let doubleLetterScore = stmntToSquareFun stmntDoubleLetterScore
let tripleLetterScore = stmntToSquareFun stmntTripleLetterScore

let doubleWordScore = stmntToSquareFun stmntDoubleWordScore
let tripleWordScore = stmntToSquareFun stmntTripleWordScore

let oddVowels = 
    stmntToSquareFun 
        (Seq (Declare "i",
            (Seq (Ass ("_result_", V "_acc_"),
                    While (V "i" .<. WL,
                            Seq(
                                ITE (IsVowel (CV (V "i")),
                                    Ass ("_result_", V "_result_" .*. N -1),
                                    Skip),
                                Ass ("i", V "i" .+. N 1)))))))


printfn "Testing stmntToSquareFun"
printfn "%A" (singleLetterScore hello 0 0)
printfn "%A" (doubleLetterScore hello 0 0)
printfn "%A" (tripleLetterScore hello 0 0)
printfn "%A" (singleLetterScore hello 0 42)
printfn "%A" (doubleLetterScore hello 0 42)
printfn "%A" (tripleLetterScore hello 0 42)
printfn "%A" (oddVowels hello 5 50)
printfn "%A" (oddVowels [('H', 4); ('E', 1); ('L', 1)] 5 50)
printfn ""
    
let emptyBoard = Skip
let singleLetterBoard = Ass ("_result_", N 0)

let abs v result = ITE (v .<. N 0, Ass (result, v .*. N -1), Ass (result, v))

let twsCheck x y = ((V x .=. N 0) .&&. (V y .=. N 7)) .||.
                    ((V x .=. N 7) .&&. ((V y .=. N 7) .||. (V y .=. N 0)))

let dwsCheck x y = (V x .=. V y) .&&. (V x .<. N 7) .&&. (V x .>. N 2)

let tlsCheck x y = ((V x .=. N 6) .&&. (V y .=. N 2)) .||.
                    ((V x .=. N 2) .&&. ((V y .=. N 2) .||. (V y .=. N 6)))

let dlsCheck x y = ((V x .=. N 0) .&&. (V y .=. N 4)) .||.
                    ((V x .=. N 1) .&&. ((V y .=. N 1) .||. (V y .=. N 5))) .||.
                    ((V x .=. N 4) .&&. ((V y .=. N 0) .||. (V y .=. N 7))) .||.
                    ((V x .=. N 5) .&&. (V y .=. N 1)) .||.
                    ((V x .=. N 7) .&&. (V y .=. N 4))

let insideCheck x y = ((V x .<. N 8) .&&. (V y .<. N 8))

let checkSquare f v els = ITE (f "xabs" "yabs", Ass ("_result_", N v), els)
    
let standardBoardFun =
    Seq (Declare "xabs",
            Seq (Declare "yabs",
                Seq (abs (V "_x_") "xabs",
                    Seq (abs (V "_y_") "yabs",
                        checkSquare twsCheck 4 
                            (checkSquare dwsCheck 3 
                                (checkSquare tlsCheck 2 
                                    (checkSquare dlsCheck 1
                                        (checkSquare insideCheck 0
                                            (Ass ("_result_", N -1))))))))))

let boardMap = [(0, singleLetterScore); (1, doubleLetterScore); (2, tripleLetterScore); 
                (3, doubleWordScore); (4, tripleWordScore)] |> Map.ofList

let evalSquare w pos acc =
    function
    | Success (Some f) -> 
        match f w pos acc with
        | Success res -> Success (Some res)
        | Failure e   -> Failure e
    | Success None -> Success None
    | Failure e        -> Failure e

let toString =
    function
    | Success (Some x) -> string x
    | Success None    -> "#"
    | Failure err     -> failwith (sprintf "Error: %A" err)

printfn "Testing stmntToBoardFun"
(* Expceted output:
    # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # # 
    # # # 9 4 4 5 4 4 4 9 4 4 4 5 4 4 9 # # # 
    # # # 4 6 4 4 4 6 4 4 4 6 4 4 4 6 4 # # # 
    # # # 4 4 6 4 4 4 5 4 5 4 4 4 6 4 4 # # # 
    # # # 5 4 4 6 4 4 4 5 4 4 4 6 4 4 5 # # # 
    # # # 4 4 4 4 6 4 4 4 4 4 6 4 4 4 4 # # # 
    # # # 4 6 4 4 4 6 4 4 4 6 4 4 4 6 4 # # # 
    # # # 4 4 5 4 4 4 5 4 5 4 4 4 5 4 4 # # # 
    # # # 9 4 4 5 4 4 4 4 4 4 4 5 4 4 9 # # # 
    # # # 4 4 5 4 4 4 5 4 5 4 4 4 5 4 4 # # # 
    # # # 4 6 4 4 4 6 4 4 4 6 4 4 4 6 4 # # # 
    # # # 4 4 4 4 6 4 4 4 4 4 6 4 4 4 4 # # # 
    # # # 5 4 4 6 4 4 4 5 4 4 4 6 4 4 5 # # # 
    # # # 4 4 6 4 4 4 5 4 5 4 4 4 6 4 4 # # # 
    # # # 4 6 4 4 4 6 4 4 4 6 4 4 4 6 4 # # # 
    # # # 9 4 4 5 4 4 4 9 4 4 4 5 4 4 9 # # # 
    # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # #     
*)

for y in -10..10 do
    for x in -10..10 do
        printf "%s " (stmntToBoardFun standardBoardFun boardMap (x, y) |> evalSquare hello 1 3 |> toString)
    printfn ""

let ids = [(0, stmntSingleLetterScore); (1, stmntDoubleLetterScore); (2, stmntTripleLetterScore); 
            (3, stmntDoubleWordScore); (4, stmntTripleWordScore)]
printfn ""


printfn "Testing mkBoard"
let standardBoard = 
    mkBoard (0, 0) stmntSingleLetterScore standardBoardFun ids


for y in -10..10 do
    for x in -10..10 do
        printf "%s " (standardBoard.squares (x, y) |> evalSquare hello 1 3 |> toString)
    printfn ""

0 // return an integer exit code



int 'H' - int '0' // his one method is fucked and has fogotten to take this into consideration
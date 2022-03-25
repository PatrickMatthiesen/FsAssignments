module Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H', 4); ('E',1);('L',1);('L',1);('O',1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b = 
        a >>= fun x ->
        b >>= fun y ->
        ret (x+y)

    let div a b = 
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0 then
            ret (x/y)
        else
            fail DivisionByZero

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsDigit of cExp      (* check for digit *)
       | IsLetter of cExp     (* check for letter *)
       | IsVowel of cExp      (* check for vowel *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    
    
    open System

    let binop f a b =
        a >>= fun x ->
        b >>= fun y ->
        ret (f x y)

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V x -> lookup x
        | WL -> wordLength
        | PV a1 -> arithEval a1 >>= (fun r -> pointValue r)
        | Add (a1, a2) -> binop ( + ) (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> binop ( - ) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop ( * ) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1,a2) -> arithEval a2 >>= fun r2 -> if r2 <> 0 then arithEval a1 >>= (fun r1 -> ret (r1%r2)) else fail DivisionByZero
        | CharToInt c -> charEval c >>= (fun r -> ret (int r)) // well i think there is a mistake in the tests the right answer would be: int r - int '0'

    and charEval c : SM<char> = 
        match c with
        | C(c) -> ret c
        | ToUpper(c) -> charEval c >>= (fun r -> ret (Char.ToUpper r))
        | ToLower(c) -> charEval c >>= (fun r -> ret (Char.ToLower r))
        | CV(a) -> arithEval a >>= (fun r -> characterValue r)
        | IntToChar a -> arithEval a >>= (fun r -> ret (char (r + int '0'))) 

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false

        | AEq(a1,a2) -> binop ( = ) (arithEval a1) (arithEval a2)
        | ALt(a1,a2) -> binop ( < ) (arithEval a1) (arithEval a2)

        | Not(b1) -> boolEval b1 >>= (fun r -> ret (not r))
        | Conj(b1, b2) -> binop ( && ) (boolEval b1) (boolEval b2)

        | IsDigit(c) -> charEval c >>= (fun r -> ret (Char.IsDigit r))
        | IsLetter(c) -> charEval c >>= (fun r -> ret (Char.IsLetter r))
        | IsVowel(c) -> charEval c >>= (fun r -> ret ("aeiouæøåAEIOUÆØÅ ".Contains r)) 


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Skip                    (* nop *)
    | Ass of string * aExp    (* variable assignment *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stm : SM<unit> = 
        match stm with
        | Declare s -> declare s
        | Skip -> ret ()
        | Ass (str,a) -> arithEval a >>= (fun r -> update str r) 
        | Seq (stm1,stm2) -> stmntEval stm1 >>>= stmntEval stm2 >>= (fun r -> ret r)
        | ITE (b, stm1, stm2) -> push >>>= boolEval b >>= (fun bool -> if bool then stmntEval stm1 else stmntEval stm2 ) >>>= pop
        | While (b,stm1) -> push >>>= boolEval b >>= (fun r -> if r then stmntEval stm1 >>>= stmntEval (While (b,stm1)) else ret ()) >>>= pop


(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let binop2 f a b =
        prog { let! x = a
               let! y = b
               return f x y }

    let rec arithEval2 a = //arithEval a
        match a with
        | N n -> prog {return n}
        | V x -> prog {return! lookup x}
        | WL -> prog {return! wordLength}
        | PV a1 -> prog {
            let! a = arithEval a1 
            return! pointValue a
            }
        | Add (a1, a2) -> binop2 ( + ) (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> binop2 ( - ) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop2 ( * ) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> prog {
                let! a = arithEval a1
                let! b = arithEval a2
                if b <> 0 then
                    return (a/b)
                else
                    return! fail DivisionByZero
            }
        | Mod (a1,a2) ->
            prog {
                let! a = arithEval a1
                let! b = arithEval a2
                if b <> 0 then
                    return (a%b)
                else
                    return! fail DivisionByZero
            }
        | CharToInt c -> prog {
                let! r = charEval c
                return int r
            }
        

    let charEval2 c = charEval c
    let rec boolEval2 b = boolEval b

    let stmntEval2 stm = //stmntEval stm
        match stm with
        | Declare s -> prog { return! declare s }
        | Skip -> prog { return () }
        | Ass (str,a1) -> 
            prog { 
                let! a = arithEval a1
                return! update str a
            } 
        | Seq (stm1,stm2) -> //stmntEval stm1 >>>= stmntEval stm2 >>= (fun r -> ret r)
            prog {
                do! stmntEval stm1
                let! a = stmntEval stm2
                return a
            }
        | ITE (b, stm1, stm2) -> //boolEval b >>= (fun bool -> if bool then stmntEval stm1 else stmntEval stm2 )
            prog {
                let! bool = boolEval b
                do! push
                if bool 
                then do! stmntEval stm1
                else do! stmntEval stm2
                do! pop
            }
        | While (b,stm1) -> //boolEval b >>= fun r -> if r then stmntEval stm1 >>>= stmntEval (While (b,stm1)) else ret ()
            prog {
                let! bool = boolEval b
                do! push
                if bool 
                then 
                    do! stmntEval stm1
                    do! stmntEval (While (b,stm1))
                else return ()
                do! pop
            }

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm : squareFun = 
        fun w pos acc -> 
            stmntEval2 stm >>>= lookup "_result_" |> 
                evalSM (
                    mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"] )


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = //failwith "Not implemented"
        fun (x,y) -> 
            stmntEval2 stm >>>= lookup "_result_" |> 
                    evalSM (
                        mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"] ) |>
                            function
                            | Success int -> if Map.containsKey int m then Success (Some m.[int]) else Success None
                            | Failure err -> Failure err

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt (ids:  (int * stm) list) = 
        {
            center = c;
            defaultSquare = stmntToSquareFun defaultSq;
            squares = stmntToBoardFun boardStmnt (List.map (fun (k,sq) -> (k, stmntToSquareFun sq)) ids |> Map.ofList)
        }
    
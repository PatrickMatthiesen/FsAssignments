module ImpParser

    open Eval
    open System

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring  "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many (pchar ' ' <|> pchar '\n') <?> "spaces"
    let spaces1        = many1 (pchar ' ') <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2 // tager og parser p1 og gemmer svaret, dernæst parserden spaces men smider resultatet ud, der efter tager den og parser p2 og smider det ud også
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' // incorrect (not implemented)
    let braces p = pchar '{' >*>. p .>*> pchar '}'

    let pid = 
        (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_') |>> (fun (x,y) -> (x::y) |> String.Concat)

    
    let unop op p1 = op >*>. p1

    let binop op p1 p2 = p1 .>*> op .>*>. p2
    
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CtomParse, cref = createParserForwardedToRef<cExp>()
    let BconParse, conref = createParserForwardedToRef<bExp>()
    let BequParse, eref = createParserForwardedToRef<bExp>()
    let BtomParse, bref = createParserForwardedToRef<bExp>()
    let StmParse, sref = createParserForwardedToRef<stm>()
    let SseqParse, seqref = createParserForwardedToRef<stm>()

    //arith
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref.Value <- choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref.Value <- choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> (N -1, x)) |>> Mul <?> "Neg"
    let VParse   = pid |>> V <?> "Variable"
    let PVParse  = unop pPointValue AtomParse |>> PV <?> "PV"
    let CharToInt= unop pCharToInt CtomParse |>> CharToInt <?> "CharToInt" 
    do aref.Value <- choice [CharToInt; NegParse; NParse; PVParse; VParse; ParParse]

    let AexpParse = TermParse 

    //char
    let CParParse = parenthesise CtomParse
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    let CVParse = unop pCharValue AtomParse |>> CV <?> "CV"
    let ToLowerParse = unop pToLower CtomParse |>> ToLower <?> "ToLower"
    let ToUpperParse = unop pToUpper CtomParse |>> ToUpper <?> "ToLower"
    let IntToCharParse = unop pIntToChar AtomParse |>> IntToChar <?> "InToChar"
    do cref.Value <- choice [ToLowerParse; ToUpperParse; IntToCharParse; CParse; CVParse; CParParse]

    let CexpParse = CtomParse

    //bool
    let BConj = binop (pstring "/\\") BequParse BconParse |>> Conj
    let BOr = binop (pstring "\\/") BequParse BconParse |>> (fun (x,y) -> x.||.y)
    do conref.Value <- choice [BConj; BOr; BequParse]


    let Bequals = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "AEq"
    let BnotEquals = binop (pstring "<>") AexpParse AexpParse |>> (fun (x,y) -> x.<>.y)
    let BLess = binop (pchar '<') AexpParse AexpParse |>> ALt
    let BLessEqual = binop (pstring "<=") AexpParse AexpParse |>> (fun (x,y) -> x.<=.y)
    let BMore = binop (pchar '>') AexpParse AexpParse |>> (fun (x,y) -> x.>.y)
    let BMoreEqual = binop (pstring ">=") AexpParse AexpParse |>> (fun (x,y) -> x.>=.y)
    do eref.Value <- choice [Bequals; BnotEquals; BLess; BLessEqual; BMore; BMoreEqual; BtomParse]

    let BParParse = parenthesise BconParse
    let BTrue = pTrue |>> (fun _ -> TT) <?> "TT"
    let BFalse = pFalse |>> (fun _ -> FF) <?> "FF"
    let BIsDigit = unop pIsDigit CtomParse |>> IsDigit <?> "IsDigit"
    let BIsLetter = unop pIsLetter CtomParse |>> IsLetter <?> "IsLetter"
    let BISVowel = unop pIsVowel CtomParse |>> IsVowel <?> "IsVowel"
    let BNot = unop (pchar '~') BconParse |>> Not
    do bref.Value <- choice [BTrue; BFalse; BIsDigit; BIsLetter; BISVowel; BNot; BParParse]

    let BexpParse = BconParse


    let Sseq = binop (pchar ';') StmParse SseqParse |>> Seq <?> "Seq"
    do seqref.Value <- choice [Sseq; StmParse]

    let SBrace = braces SseqParse
    let Sass = binop (pstring ":=") pid AexpParse |>> Ass
    let Sdeclair = pdeclare >>. whitespaceChar >>. pid |>> Declare <?> "Declare"
    let Site = pif >*>. BParParse .>*> pthen .>*>. SBrace .>*> pelse .>*>. SBrace |>> (fun ((b,stm1),stm2) -> ITE (b, stm1, stm2))
    let Sit = pif >*>. BParParse .>*> pthen .>*>. SBrace |>> (fun (b,stm1) -> ITE (b,stm1, Skip))
    let Swhile = pwhile >*>. BParParse .>*> pdo .>*>. SBrace |>> While
    do sref.Value <- choice [SBrace; Sass; Sdeclair; Site; Sit; Swhile]

    let stmntParse = SseqParse

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }

    type word   = (char * int) list
    type square = Map<int, squareFun>

    let parseSquareProg (sqp: squareProg): square = sqp |> Map.map (fun k v -> run stmntParse v |> getSuccess |> stmntToSquareFun)

    let parseBoardProg (bp: boardProg): board = 

    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard (bp : boardProg) = failwith "not implemented"


module JParsec

(* Modules and datatypes inspired by Scott Wlaschin's web-site
   F# for fun and profit *)

open System

type Position = {
    line : int
    column : int
}

module Position =

    let mkPos l c   = {line = l; column = c}
    let getLine p   = p.line
    let getColumn p = p.column

    let setLine l p  = {p with line = l}
    let setColumn l p = {p with column = l}

    let initialPos = mkPos 0 0

    let incrCol p = setColumn (getColumn p + 1) p
    let incrLine p = (setLine (getLine p + 1) >> setColumn 0) p


type TextInputState = {
    lines : string[]
    position : Position 
}

module TextInputState =

    let initialInputState s =
        if String.IsNullOrEmpty(s) then
            {lines=[||]; position=Position.initialPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = s.Split(separators, StringSplitOptions.None)
            {lines=lines; position=Position.initialPos} 

    let getLines    is = is.lines
    let getPosition is = is.position

    let setPosition is p = {is with position = p}

    let numLines = getLines >> Array.length

    let columnNumber = getPosition >> Position.getColumn
    let lineNumber   = getPosition >> Position.getLine

    let getLine is = 
        function
        | i when i < numLines is -> Some (getLines is).[i]
        | _ -> None

    let currentLine is = 
        getPosition is |> 
        Position.getLine |> 
        getLine is

    let getError is =
        let failureCaret = sprintf "%*s^" (columnNumber is) ""
        sprintf "Line: %i\tColumn: %i\n%s\n%s" 
                (lineNumber is) (columnNumber is) 
                (currentLine is |> Option.get) failureCaret

    let incrCol  is = getPosition is |> Position.incrCol |> setPosition is
    let incrLine is = getPosition is |> Position.incrLine |> setPosition is

    let nextChar is =
        let cPos = columnNumber is

        match currentLine is with
        | Some l when cPos < l.Length -> incrCol is, Some l.[cPos]
        | Some l                      -> incrLine is, Some '\n'
        | None                        -> is, None                


type ParseError = {
    label : string
    input : TextInputState
    error : string
}
with override this.ToString() =
                sprintf "Error parsing %s\n%s%s" 
                        this.label (TextInputState.getError this.input) (this.error)

module ParseError =
    let mkParseError l i err = {label = l; input = i; error = err}

    let setLabel pe l = {pe with label = l}

    let getLabel pe = pe.label
    let getInput pe = pe.input
    let getError pe = pe.error

    let print pe = 
        sprintf "Error parsing %s\n%s%s" 
                (getLabel pe) (TextInputState.getError (getInput pe)) (getError pe)


type ParserResult<'a> =
    | Success of 'a
    | Failure of ParseError
    with override this.ToString() =
                    match this with
                    | Success a -> sprintf "Success: %A\n" a
                    | Failure e  -> sprintf "Failure:\n%A\n" e

                
module TextParser =
        
    type Parser<'a> = {
       pfun  : TextInputState -> ParserResult<'a * TextInputState>
       label : string
    }

    let mkParser f l = {pfun = f; label = l}

    let getLabel p = p.label

    let runParser parser input =
        parser.pfun input
        
    let satisfy predicate =
        let parseFun input =
            let remainingInput, symOpt = TextInputState.nextChar input
            match symOpt with
            | None     -> Failure (ParseError.mkParseError "satisfy" input "no more input")
            | Some sym when predicate sym -> 
                  Success (sym, remainingInput)
            | Some sym -> 
                let err = sprintf "unexpected %A\n" sym
                Failure (ParseError.mkParseError "satisfy" input err)

        mkParser parseFun "satisfy"

    let returnP x = mkParser (fun input -> Success(x, input)) "unknown"

    let bindP f p = 
        let parseFun input =
            match runParser p input with
            | Failure pe -> Failure pe
            | Success (v, remainingInput) ->
                runParser (f v) remainingInput
        mkParser parseFun "unknown"


    let (>>=) p f = bindP f p

    let setLabel p l =
        let parseFun input =
            match runParser p input with
            | Success s  -> Success s
            | Failure pe -> Failure (ParseError.setLabel pe l)

        mkParser parseFun l

    let (<?>) p l = setLabel p l

    let andThen p1 p2 = 
        p1 >>= (fun r1 -> p2 >>= fun r2 -> returnP (r1, r2)) <?>
        sprintf "(%s andThen %s)" (getLabel p1) (getLabel p2)

    let (.>>.) p1 p2 = andThen p1 p2
        
    let orElse p1 p2 =
        let label = sprintf "(%s orElse %s)" (getLabel p1) (getLabel p2)
        let parserFun input =
            match runParser p1 input with
            | Success s  -> Success s
            | Failure pe -> runParser p2 input

        mkParser parserFun label

    let (<|>) p1 p2 = orElse p1 p2

    let applyP fP xP =
        fP >>= (fun f -> xP >>= (fun x -> returnP (f x)))

    let ( <*> ) fP xP = applyP fP xP

    let choice ps = Seq.reduce orElse ps

    let mapP f = bindP (f >> returnP)

    let (|>>) p f = mapP f p

    let parseZeroOrMore p input = 
        let rec aux input acc =
            match runParser p input with
            | Failure pe -> Success (List.rev acc, input)
            | Success (v, remainingInput) ->
                aux remainingInput (v::acc)

        aux input []

    let many p = mkParser (parseZeroOrMore p) "unknown"

    let many1 p = 
        p >>= fun x -> many p >>= fun xs -> returnP (x :: xs)

    let rec sequence =
        function
        | []      -> returnP []
        | x :: xs -> returnP (fun h tl -> h :: tl) <*> x <*> (sequence xs)
        
    let opt p = p |>> Some <|> returnP None

    let (.>>) p1 p2 = p1 .>>. p2 |>> fst
    let (>>.) p1 p2 = p1 .>>. p2 |>> snd
    let between popen pclose p = popen >>. p .>> pclose

    let sepBy1 p sep =
        p .>>. many (sep >>. p)
        |>> fun (x,xs) -> x::xs

    let sepBy p sep =
        sepBy1 p sep <|> returnP []

    let createParserForwardedToRef<'a> () =

        let dummyParser= 
            let innerFn input : ParserResult<'a * TextInputState> = failwith "unfixed forwarded parser"
            mkParser innerFn "unknown"

        let parserRef = ref dummyParser 

        let innerFn input = runParser parserRef.Value input 

        mkParser innerFn "unknown", parserRef


    let charListToStr charList = String(List.toArray charList) |> string

    let anyChar = satisfy (fun _ -> true) <?> "anyChar"

    let pchar c = satisfy (fun x -> x = c) <?> (sprintf "%c" c)

    let asciiLetter = satisfy Char.IsLetter <?> "letter"

    let anyOf ss =
        ss |> List.map pchar |> choice <?> 
        sprintf "anyOf %A" ss

    let manyChars p  = many p  |>> charListToStr
    let manyChars1 p = many1 p |>> charListToStr

    let pstring str =
        str |>
        List.ofSeq |>
        List.map pchar |>
        sequence |>>
        charListToStr <?>
        str

    let digit = satisfy Char.IsDigit <?> "digit"

    let digits = manyChars1 digit

    let pint32 =
        opt (pchar '-') .>>. digits |>>
        function
        | (Some _, ds) -> -(int ds)
        | (None, ds)   -> int ds

    let run parser inputStr =
        runParser parser (TextInputState.initialInputState inputStr) |>
        function
        | Success (a, _) -> Success a
        | Failure pe  -> Failure pe

    let runParserFromFile parser path =
        run parser (System.IO.File.ReadAllText path)

    let getSuccess =
        function
        | Success a -> a
        | failure   -> failwith (sprintf "%A" failure)

    

module Gaddag
    open System.Collections.Generic
    type Dict = 
        | Leaf of bool
        | Node of bool * Dictionary<char, Dict>
    let empty () = Leaf false

    let ancor = ">"

    let rec insert (s:string) (d:Dict) =
        let _split = 
            [for i in 0..(s.Length-1) -> 
                (s.Substring(0,i+1) |> Array.ofSeq |> Array.rev |> System.String) + $"{ancor}{s[i+1..]}"
            ]

        let rec _insert (s:string) =
            function
            | Leaf _ when s.Length = 0 -> 
                Leaf true
            | Node (_, dict) when s.Length = 0 -> 
                Node(true, dict)

            | Leaf b ->
                let dict = Dictionary ()
                dict.[s[0]] <- _insert s[1..] (empty ())
                Node (b, dict)

            | Node (b, dict) ->
                match dict.TryGetValue s[0] with
                | (true, d) ->
                    dict.[s[0]] <- _insert s[1..] d
                    Node (b, dict)
                | (false, _) ->
                    dict.[s[0]] <- _insert s[1..] (empty ())
                    Node (b, dict)

        let words = _split 
        List.fold (fun acc word -> _insert word acc) d words

    let rec lookup (s:string) =
        let aux (s:string) = 
            function
            | Leaf b when s.Length = 0 -> b
            | Leaf _ -> false
            | Node (b, _) when s.Length = 0 -> b
            | Node (_, dict) ->
                match dict.TryGetValue s[0] with
                | (true, d) -> lookup s[1..] d
                | (false, _) -> false 
        aux (s[0].ToString() + $"{ancor}{s[1..]}")

    let rec step c =
        function 
        | Node (_, dict) -> 
            match dict.TryGetValue c with 
            | (true, value) ->
                match value with
                | Leaf b -> Some (b, value)
                | Node (b,_) -> Some (b,value)
            | (false, _) -> None
        | Leaf _ -> None

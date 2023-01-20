// This is my Trie implementation.
// You can find the Gaddag implementation under 'Project/Gaddag.fs'
// If you want to make and hand in the Gaddag implementation, 
//  you might have to rename the file and or the module name.

module Dictionary
    open System.Collections.Generic
    type Dict = 
        | Leaf of bool
        | Node of bool * Dictionary<char, Dict>
    let empty () = Leaf false

    let rec insert (s:string) =
        function
        | Leaf _ when s.Length = 0 -> 
            Leaf true
        | Node (_, dict) when s.Length = 0 -> 
            Node(true, dict)

        | Leaf b ->
            let dict = Dictionary ()
            dict.[s.[0]] <- insert s.[1..] (empty ())
            Node (b, dict)

        | Node (b, dict) ->
            match dict.TryGetValue s.[0] with
            | (true, d) ->
                dict.[s.[0]] <- insert s.[1..] d
                Node (b, dict)
            | (false, _) ->
                dict.[s.[0]] <- insert s.[1..] (empty ())
                Node (b, dict)

    let rec lookup (s:string) =
        function
        | Leaf b when s.Length = 0 -> b
        | Leaf _ -> false
        | Node (b, _) when s.Length = 0 -> b
        | Node (_, dict) ->
            match dict.TryGetValue s.[0] with
            | (true, d) -> lookup s.[1..] d
            | (false, _) -> false      

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
        
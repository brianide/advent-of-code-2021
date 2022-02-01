#load "util.fsx"
open Util

type LineResult =
    | Complete
    | Incomplete
    | Corrupted of char

let classifyLine =
    let (|Open|Close|) =
        function
        | '(' -> Open ')'
        | '[' -> Open ']'
        | '{' -> Open '}'
        | '<' -> Open '>'
        | c -> Close c

    let rec helper stack input =
        match (input, stack) with
        | Open c :: rest, _ -> helper (c :: stack) rest
        | Close c :: rest, exp :: tail when c = exp -> helper tail rest
        | Close c :: _, exp :: _ when c <> exp -> Corrupted c
        | [], [] -> Complete
        | [], _ -> Incomplete
        | x -> failwithf "Invalid state: %A" x

    List.ofSeq >> helper []

let scoreLine =
    function
    | Corrupted ')' -> 3
    | Corrupted ']' -> 57
    | Corrupted '}' -> 1197
    | Corrupted '>' -> 25137
    | _ -> 0

inputLines.Value
|> List.sumBy (classifyLine >> scoreLine >> uint32)
|> printfn "%i"

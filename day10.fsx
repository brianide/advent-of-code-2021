#load "util.fsx"
open Util

type LineResult =
    | Complete
    | Incomplete of char list
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
        | Open c :: rest, stack -> helper (c :: stack) rest
        | Close c :: rest, exp :: tail when c = exp -> helper tail rest
        | Close c :: _, exp :: _ when c <> exp -> Corrupted c
        | [], [] -> Complete
        | [], stack -> Incomplete stack
        | x -> failwithf "Invalid state: %A" x

    List.ofSeq >> helper []

let scoreLine =
    let scoreChar =
        function
        | ')' -> 1UL
        | ']' -> 2UL
        | '}' -> 3UL
        | '>' -> 4UL
        | _ -> 0UL

    function
    | Corrupted ')' -> 3UL
    | Corrupted ']' -> 57UL
    | Corrupted '}' -> 1197UL
    | Corrupted '>' -> 25137UL
    | Incomplete stack -> List.fold (fun acc c -> acc * 5UL + (scoreChar c)) 0UL stack
    | _ -> 0UL

let median s = Seq.item (Seq.length s / 2) s

inputLines.Value
|> Seq.map classifyLine
|> Seq.filter (function Incomplete _ -> true | _ -> false)
|> Seq.map scoreLine
|> Seq.sort
|> median
|> printfn "%i"
#load "util.fsx"
open Util

type Orientation = Horizontal | Vertical | Diagonal

let parseInput lines =
    let rangeDiagonal (x1, y1) (x2, y2) =
        let (xstep, ystep) = (sign (x2 - x1), sign (y2 - y1))
        let limit = abs (x2 - x1)
        [for i in 0 .. limit -> (x1 + xstep * i, y1 + ystep * i)]
    let coordPairs = lines |> List.map (regSplit " -> " >> List.map (split ',' >> List.map int))
    let handlePair = function
    | [[x1; y1]; [x2; y2]] when x1 = x2 -> (Vertical, [for i in y1 .. sign (y2 - y1) .. y2 -> (x1, i)])
    | [[x1; y1]; [x2; y2]] when y1 = y2 -> (Horizontal, [for i in x1 .. sign (x2 - x1) .. x2 -> (i, y1)])
    | [[x1; y1]; [x2; y2]] -> (Diagonal, rangeDiagonal (x1, y1) (x2, y2))
    | n -> failwithf "Invalid input: %A" n
    List.map handlePair coordPairs

let solve considerDiag input =
    if considerDiag then input else List.filter (fst >> ((<>) Diagonal)) input
    |> List.collect snd
    |> List.countBy id
    |> List.filter (fun (_, count) -> count > 1)
    |> List.length

let solutions =
    let input = parseInput inputLines.Value
    (solve false input, solve true input)

printfn "%A" solutions
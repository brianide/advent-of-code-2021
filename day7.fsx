#load "util.fsx"
open Util

let positions = inputText.Value |> split ',' |> List.map int

let constantCost p d = abs (p - d)
let growingCost p d = Seq.sum (seq { 1 .. (constantCost p d) })

let solveWithScheme costScheme =
    seq { List.min positions .. List.max positions }
    |> Seq.map (fun center -> (center, List.sumBy (costScheme center) positions))
    |> Seq.minBy snd

[constantCost; growingCost]
|> List.map solveWithScheme
|> printfn "%A"
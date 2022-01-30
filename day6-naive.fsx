#load "util.fsx"
open Util

let parseInput = split ',' >> List.map int

let tickAllFish =
    let tickOneFish = function
    | 0 -> [6; 8]
    | n -> [n - 1]

    List.collect tickOneFish

let iterate n f = Seq.init n (fun _ -> f) |> Seq.reduce (>>)

inputText.Value
|> parseInput
|> iterate 80 tickAllFish
|> List.length
|> printfn "%A"
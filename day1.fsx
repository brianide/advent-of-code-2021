#load "util.fsx"
open Util

let input =
    Array.get fsi.CommandLineArgs 1
    |> System.IO.File.ReadLines
    |> List.ofSeq
    |> List.map int;;

// Part A
input
|> List.pairwise
|> List.where (fun (a, b) -> a < b)
|> List.length
|> printfn "%A"

// Part B
input
|> List.windowed 3
|> List.map List.sum
|> List.pairwise
|> List.where (fun (a, b) -> a < b)
|> List.length
|> printfn "%A"
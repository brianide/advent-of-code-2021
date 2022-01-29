#load "util.fsx"
open Util

type Board = { Grid: int list list; Winners: int Set list; Numbers: int list }

let parse (drawLine :: boardLines) =
    let draws = drawLine |> split ',' |> List.map int
    let boardNums = boardLines |> List.collect (regSplit @"\s+") |> List.filter (fun n -> String.length n > 0) |> List.map int
    let linesForBoard board =
        [ for i in 0 .. 4 do yield [ for j in 0 .. 4 -> (i, j) ]; yield [ for j in 0 .. 4 -> (j, i) ]]
        |> List.map (List.map (fun (i, j) -> board |> List.item i |> List.item j) >> Set.ofList)
    let numsToBoard nums =
        let grid = List.chunkBySize 5 nums
        let lines = linesForBoard grid
        { Grid = grid; Winners = lines; Numbers = nums }
    let boards =
        boardNums
        |> List.chunkBySize 25
        |> List.map numsToBoard
    (draws, boards)

let (draws, boards) = Array.get fsi.CommandLineArgs 1 |> System.IO.File.ReadAllLines |> List.ofArray |> parse
let rec step drawn remaining =
    let next = List.head remaining
    let newDrawn = Set.add next drawn
    let isWinner board = board.Winners |> List.exists (Set.isSuperset newDrawn)
    let calcScore board = 
        let unmarked = List.filter (fun n -> not <| Set.contains n newDrawn) board.Numbers |> List.sum
        unmarked * next
    match List.tryFind isWinner boards with
    | Some board -> (calcScore board, board)
    | None -> step newDrawn (List.tail remaining)

step Set.empty draws |> printfn "%A"
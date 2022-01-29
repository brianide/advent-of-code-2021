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

let getWinningScores boards draws =
    let reducer (playing, prevDraws, winners) latestDraw =
        let draws = Set.add latestDraw prevDraws
        let isWinner board = board.Winners |> List.exists (Set.isSuperset draws)
        let calcScore board = 
            let unmarked = List.filter (fun n -> not <| Set.contains n draws) board.Numbers |> List.sum
            unmarked * latestDraw
        let (won, lost) = List.partition isWinner playing
        (lost, draws, winners @ (List.map (fun a -> calcScore a) won))
    let (_, _, winners) = draws |> List.fold reducer (boards, Set.empty, [])
    winners

let (draws, boards) = Array.get fsi.CommandLineArgs 1 |> System.IO.File.ReadAllLines |> List.ofArray |> parse
printfn "%A" (getWinningScores boards draws)
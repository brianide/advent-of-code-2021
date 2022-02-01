#load "util.fsx"
open Util

let neighborsOf (x, y) =
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
    |> List.map (fun (i, j) -> (x + i, y + j))

let neighborValues grid p =
    let maxX = Array2D.length1 grid
    let maxY = Array2D.length2 grid

    let getCellValue (x, y) =
        if x >= 0 && x < maxX && y >= 0 && y < maxY then
            Array2D.get grid x y |> Some
        else
            None

    neighborsOf p |> List.choose getCellValue

let grid = inputLines.Value |> Seq.map (Seq.map (fun c -> int c - int '0' + 1)) |> array2D
seq { for x in 0 .. Array2D.length1 grid - 1 do
      for y in 0 .. Array2D.length2 grid - 1 do
      (x, y) }
|> Seq.map (fun (x, y) -> (Array2D.get grid x y,  neighborValues grid (x, y)))
|> Seq.filter (fun (v, neighbors) -> List.forall ((<) v) neighbors)
|> Seq.sumBy fst
|> printfn "%A"

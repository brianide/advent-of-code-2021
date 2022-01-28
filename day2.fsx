#load "util.fsx"
open Util

type Direction = Forward | Up | Down
let (|Direction|_|) = function
| "forward" -> Some Forward
| "up" -> Some Up
| "down" -> Some Down
| _ -> None

let (|Manuever|_|) line =
    match split ' ' line with
    | [Direction a; Integer b] -> Some (a, b)
    | _ -> eprintfn "Invalid line: %s" line; None

let input =
    Array.get fsi.CommandLineArgs 1
    |> System.IO.File.ReadLines
    |> List.ofSeq
    |> List.choose (|Manuever|_|);;

// Part A
let toCoord (dir, mag) =
    match dir with
    | Forward -> (mag, 0)
    | Up -> (0, -mag)
    | Down -> (0, mag)
let motionA (a, b) (x, y) = (a + x, b + y)

input
|> List.map toCoord
|> List.reduce motionA
|> fun (a, b) -> a * b
|> printfn "%A"

// Part B
type SubState = { Distance: int; Depth: int; Aim: int }
let motionB state (dir, dist) = 
    match dir with
    | Forward -> { state with
                    Distance = state.Distance + dist;
                    Depth = state.Depth + dist * state.Aim }
    | Up -> { state with Aim = state.Aim - dist }
    | Down -> { state with Aim = state.Aim + dist }

input
|> List.fold motionB { Distance = 0; Depth = 0; Aim = 0 }
|> (fun s -> {| Solution = s.Distance * s.Depth; FinalState = s |})
|> printfn "%A"
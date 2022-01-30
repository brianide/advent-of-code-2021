#load "util.fsx"
open Util

// System state is represented by a List<int>, where the value at each index in the list represents
// the number of fish with that timer value.
let initialState =
    let counts = inputText.Value |> split ',' |> List.map int |> List.countBy id
    let lookup index = counts |> List.tryFind (fst >> ((=) index)) |> Option.map snd
    List.init 9 (lookup >> Option.defaultValue 0 >> uint64)

// To determine the new value for each index when advancing the state, we map each index in the
// new state to one or more indices whose values from the previous state will be added to together.
// We also use uint64 instead of int32, because the numbers are gonna get big.
let nextState state =
    let indexMapper = function
    | 8 -> [0]
    | 6 -> [0; 7]
    | n -> [n + 1]
    let valueFor = indexMapper >> List.sumBy (fun i -> List.item i state)
    List.init 9 valueFor

let iterate n f = Seq.init n (fun _ -> f) |> Seq.reduce (>>)

initialState |> iterate 256 nextState |> List.sum |> printfn "%i"
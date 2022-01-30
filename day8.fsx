#load "util.fsx"
open Util

// Solve part 1 (lol)
let () =
    let isEasy n = List.contains (String.length n) [2; 4; 3; 7]
    inputLines.Value
    |> Seq.collect (regSplit " [|] " >> List.item 1 >> split ' ')
    |> Seq.filter isEasy
    |> Seq.length
    |> printfn "%i"

// Solve part 2
let findIn list key = List.find (fst >> ((=) key)) list |> snd

let parseLine =
    regSplit " [|] "
    >> List.map (split ' ')
    >> function
        | [a; b] -> (a, b)
        | _ -> failwith "Invalid input"

let solveWires signals =
    let fourSignal = signals |> List.find (String.length >> ((=) 4)) |> List.ofSeq
    let appearanceCounts = List.collect List.ofSeq signals |> List.countBy id
    let sigMapper (fakeSignal, count, inFour) =
        let realSignalIndex =
            match (inFour, count) with
            | (false, 8) -> 0
            | (true, 6) -> 1
            | (true, 8) -> 2
            | (true, 7) -> 3
            | (false, 4) -> 4
            | (true, 9) -> 5
            | (false, 7) -> 6
            | n -> failwithf "Invalid signature: %A" n
        (fakeSignal, pown 2 realSignalIndex)
    appearanceCounts
    |> List.map (fun (seg, count) -> (seg, count, List.contains seg fourSignal))
    |> List.map sigMapper

let solveDigits signals =
    let wireMappings = solveWires signals
    let mapDigitSignature = function
        | 0b1110111 -> 0
        | 0b0100100 -> 1
        | 0b1011101 -> 2
        | 0b1101101 -> 3
        | 0b0101110 -> 4
        | 0b1101011 -> 5
        | 0b1111011 -> 6
        | 0b0100101 -> 7
        | 0b1111111 -> 8
        | 0b1101111 -> 9
        | n -> failwithf "Invalid value: %i" n
    let getRealDigit = Seq.sumBy (findIn wireMappings) >> mapDigitSignature
    List.map (fun n -> (Set.ofSeq n, getRealDigit n)) signals

let solveLine (signals, outputs) = 
    let digitMappings = solveDigits signals
    outputs
    |> Seq.map (Set.ofSeq >> findIn digitMappings)
    |> Seq.reduce ((*) 10 >> (+))

inputLines.Value
|> List.sumBy (parseLine >> solveLine)
|> printfn "%A"
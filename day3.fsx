let zipmap f a b = Seq.zip a b |> Seq.map (fun (a, b) -> f a b) 

let input =
    Array.get fsi.CommandLineArgs 1
    |> System.IO.File.ReadAllLines

let toDecimal b =
    let length = (Array.head input |> String.length)
    let powers = Seq.init length (pown 2) |> Seq.rev
    b |> zipmap (*) powers |> Seq.sum

let toDigits = function '0' -> 0 | '1' -> 1 | n -> failwithf "Invalid digit: %A" n

let ones =
    List.ofSeq input
    |> List.map (fun s -> seq s |> Seq.map toDigits)
    |> List.reduce (zipmap (+))

let half = Array.length input / 2

let gamma =
    ones
    |> Seq.map (fun a -> if a > half then 1 else 0)
    |> toDecimal

let epsilon = 
    ones
    |> Seq.map (fun a -> if a < half then 1 else 0)
    |> toDecimal

let consumption = gamma * epsilon

printfn "%A" consumption
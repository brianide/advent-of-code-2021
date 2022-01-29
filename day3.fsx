let zipmap f a b = Seq.zip a b |> Seq.map (fun (a, b) -> f a b) 

let input =
    Array.get fsi.CommandLineArgs 1
    |> System.IO.File.ReadAllLines

let powers = 
    let length = (Array.get input 0 |> String.length) - 1
    seq { for i in 0 .. length -> pown 2 i } |> Seq.rev

let toDigits = function '0' -> 0 | '1' -> 1 | n -> failwithf "Invalid digit: %A" n

let ones =
    List.ofSeq input
    |> List.map (fun s -> seq s |> Seq.map toDigits)
    |> List.reduce (zipmap (fun a b -> a + b))

let half = Array.length input / 2

let gamma =
    ones
    |> Seq.map (fun a -> if a > half then 1 else 0)
    |> zipmap (fun a b -> a * b) powers
    |> Seq.sum

let epsilon = 
    ones
    |> Seq.map (fun a -> if a < half then 1 else 0)
    |> zipmap (fun a b -> a * b) powers
    |> Seq.sum

let consumption = gamma * epsilon

printfn "%A" consumption
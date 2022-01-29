let zipmap f a b = Seq.zip a b |> Seq.map (fun (a, b) -> f a b)
let joinToString a = Seq.map string a |> Seq.reduce (+)

let input =
    let stringToBits = Seq.map (function '0' -> 0 | '1' -> 1 | n -> failwithf "Invalid digit: %A" n)
    Array.get fsi.CommandLineArgs 1
    |> System.IO.File.ReadAllLines
    |> Seq.map stringToBits

let part1Solution =
    let ones = input |> Seq.reduce (zipmap (+))
    let half = Seq.length input / 2

    let (gamma, epsilon) =
        let powers = Seq.initInfinite (pown 2)
        let (gt, lte) =
            Seq.zip (Seq.rev ones) powers
            |> List.ofSeq
            |> List.partition (fun (coeff, _) -> coeff > half)

        let sumPowers = List.sumBy snd
        (sumPowers gt, sumPowers lte)

    gamma * epsilon

let part2Solution =
    let arrays = input |> Seq.map Seq.toArray
    
    let rateByCriteria p =
        let toDecimal s =
            let powers = Seq.initInfinite (pown 2)
            Seq.zip (Seq.rev s) powers
            |> Seq.filter (fst >> ((=) 1))
            |> Seq.sumBy snd

        let rec step rows digitIndex =
            if Seq.length rows = 1 then
                Seq.head rows
            else
                let candidates = rows |> Seq.map (fun r -> (r, Array.get r digitIndex))
                let isKeeper =
                    let oneCount = candidates |> Seq.sumBy snd
                    let zeroCount = (Seq.length rows) - oneCount
                    let desired = if (p oneCount zeroCount) then 1 else 0
                    fun (_, digit) -> digit = desired
                let newIndices = Seq.filter isKeeper candidates |> Seq.map fst
                step newIndices (digitIndex + 1)

        step arrays 0 |> toDecimal
    rateByCriteria (>=) * rateByCriteria (<)

printfn "%A %A" part1Solution part2Solution
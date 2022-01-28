let split delim str = (str: string).Split [|delim|] |> Array.toList

let (|Integer|_|) (x: string) = try int x |> Some with :? System.FormatException -> None 

module List =
    let windowed2 list =
        list
        |> List.windowed 2
        |> List.map (function [a; b] -> (a, b) | _ -> failwith "Invalid state")
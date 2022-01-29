let split delim str = (str: string).Split [|delim|] |> Array.toList

let (|Integer|_|) (x: string) = try int x |> Some with :? System.FormatException -> None 
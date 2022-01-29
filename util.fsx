let split delim str = (str: string).Split [|delim|] |> Array.toList
let regSplit reg str = System.Text.RegularExpressions.Regex.Split (str, reg) |> Array.toList

let (|Integer|_|) (x: string) = try int x |> Some with :? System.FormatException -> None 
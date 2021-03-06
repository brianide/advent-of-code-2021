let split delim str = (str: string).Split [|delim|] |> Array.toList
let regSplit reg str = System.Text.RegularExpressions.Regex.Split (str, reg) |> Array.toList

let (|Integer|_|) (x: string) = try int x |> Some with :? System.FormatException -> None 

let inputText = lazy (Array.get fsi.CommandLineArgs 1 |> System.IO.File.ReadAllText)
let inputLines = lazy (Array.get fsi.CommandLineArgs 1 |> System.IO.File.ReadAllLines |> List.ofArray)
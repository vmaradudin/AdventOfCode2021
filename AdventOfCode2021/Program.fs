open System
open AdventOfCode2021

[<EntryPoint>]
printfn "start"

Day2.input |> Seq.ofArray
|> Day2.puzzle2
|> printfn "%d"

printfn "done"
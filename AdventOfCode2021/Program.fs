open AdventOfCode2021
open Common

[<EntryPoint>]

printfn "Day 1"
InputReader.readFile @"inputs\Input1.txt" 
|> InputReader.toInt
|> fun i -> Day1.puzzle1 i, Day1.puzzle2 i
||> printfn "Puzzle 1: %d    Puzzle 2: %d"

printfn "Day 2"
InputReader.readFile @"inputs\Input2.txt"
|> Seq.ofArray
|> fun i -> Day2.puzzle1 i, Day2.puzzle2 i
||> printfn "Puzzle 1: %d    Puzzle 2: %d"
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

printfn "Day 3"
InputReader.readFile @"inputs\Input3.txt"
|> Seq.ofArray
|> fun i -> Day3.puzzle1 i, Day3.puzzle2 i
||> printfn "Puzzle 1: %d    Puzzle 2: %d"

printfn "Day 4"
InputReader.readFile @"inputs\Input4.txt"
|> fun i -> Day4.puzzle1 i, Day4.puzzle2 i
||> printfn "Puzzle 1: %d    Puzzle 2: %d"

printfn "Day 5"
InputReader.readFile @"inputs\Input5.txt"
|> fun i -> Day5.puzzle1 i, Day5.puzzle2 i
||> printfn "Puzzle 1: %d   Puzzle 2: %d"

printfn "Day 6"
InputReader.readFile @"inputs\Input6.txt"
|> Array.head
|> fun i -> Day6.puzzle1 i, Day6.puzzle2 i
||> printfn "Puzzle 1: %d   Puzzle 2: %d"

printfn "Day 7"
InputReader.readFile @"inputs\Input7.txt"
|> Array.head
|> fun i -> Day7.puzzle1 i, Day7.puzzle2 i
||> printfn "Puzzle 1: %d   Puzzle 2: %d"

printfn "Day 8"
InputReader.readFile @"inputs\Input8.txt"
|> fun i -> Day8.puzzle1 i, Day8.puzzle2 i
||> printfn "Puzzle 1: %d   Puzzle 2: %d"

printfn "Day 9"
InputReader.readFile @"inputs\Input9.txt"
|> fun i -> Day9.puzzle1 i, Day9.puzzle2 i
||> printfn "Puzzle 1: %d   Puzzle 2: %d"

printfn "Day 10"
InputReader.readFile @"inputs\Input10.txt"
|> fun i -> Day10.puzzle1 i, Day10.puzzle2 i
||> printfn "Puzzle 1: %d   Puzzle 2: %d"

printfn "Day 11"
InputReader.readFile @"inputs\Input11.txt"
|> fun i -> Day11.puzzle1 i, Day11.puzzle2 i
||> printfn "Puzzle 1: %d   Puzzle 2: %d"
open AdventOfCode2021
open Common

[<EntryPoint>]

InputReader.readFile @"inputs\Input1.txt" 
|> InputReader.toInt
|> fun i -> Day01.puzzle1 i, Day01.puzzle2 i
||> printfn "Day  1 | Puzzle 1: %d    Puzzle 2: %d"

InputReader.readFile @"inputs\Input2.txt"
|> Seq.ofArray
|> fun i -> Day02.puzzle1 i, Day02.puzzle2 i
||> printfn "Day  2 | Puzzle 1: %d    Puzzle 2: %d"

InputReader.readFile @"inputs\Input3.txt"
|> Seq.ofArray
|> fun i -> Day03.puzzle1 i, Day03.puzzle2 i
||> printfn "Day  3 | Puzzle 1: %d    Puzzle 2: %d"

InputReader.readFile @"inputs\Input4.txt"
|> fun i -> Day04.puzzle1 i, Day04.puzzle2 i
||> printfn "Day  4 | Puzzle 1: %d    Puzzle 2: %d"

InputReader.readFile @"inputs\Input5.txt"
|> fun i -> Day05.puzzle1 i, Day05.puzzle2 i
||> printfn "Day  5 | Puzzle 1: %d   Puzzle 2: %d"

InputReader.readFile @"inputs\Input6.txt"
|> Array.head
|> fun i -> Day06.puzzle1 i, Day06.puzzle2 i
||> printfn "Day  6 | Puzzle 1: %d   Puzzle 2: %d"

InputReader.readFile @"inputs\Input7.txt"
|> Array.head
|> fun i -> Day07.puzzle1 i, Day07.puzzle2 i
||> printfn "Day  7 | Puzzle 1: %d   Puzzle 2: %d"

InputReader.readFile @"inputs\Input8.txt"
|> fun i -> Day08.puzzle1 i, Day08.puzzle2 i
||> printfn "Day  8 | Puzzle 1: %d   Puzzle 2: %d"

InputReader.readFile @"inputs\Input9.txt"
|> fun i -> Day09.puzzle1 i, Day09.puzzle2 i
||> printfn "Day  9 | Puzzle 1: %d   Puzzle 2: %d"

InputReader.readFile @"inputs\Input10.txt"
|> fun i -> Day10.puzzle1 i, Day10.puzzle2 i
||> printfn "Day 10 | Puzzle 1: %d   Puzzle 2: %d"

InputReader.readFile @"inputs\Input11.txt"
|> fun i -> Day11.puzzle1 i, Day11.puzzle2 i
||> printfn "Day 11 | Puzzle 1: %d   Puzzle 2: %d"

InputReader.readFile @"inputs\Input12.txt"
|> fun i -> Day12.puzzle1 i, Day12.puzzle2 i
||> printfn "Day 12 | Puzzle 1: %d   Puzzle 2: %d"
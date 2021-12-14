namespace AdventOfCode2021

open Common.Types

module Day01 =
    
    let calculate windowSize (array:string[])=
        array
        |> Seq.map int
        |> Seq.windowed windowSize
        |> Seq.map Array.sum
        |> Seq.pairwise
        |> Seq.countBy(fun (a,b) -> a<b)
        |> Seq.find fst
        |> snd

    let puzzle1 input = input |> calculate 1

    let puzzle2 input = input |> calculate 3

    let Solution = new Solution(1, puzzle1, puzzle2)
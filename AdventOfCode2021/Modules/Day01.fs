namespace AdventOfCode2021

module Day01 =

    let compute (a:int, b:int) = if a < b then 1 else 0
    
    let puzzle1 seq = seq |> Seq.pairwise |> Seq.map compute |> Seq.sum 

    let puzzle2 seq = seq |> Seq.windowed 3 |> Seq.map Array.sum |> puzzle1
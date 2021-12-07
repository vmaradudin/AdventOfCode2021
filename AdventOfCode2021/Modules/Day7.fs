namespace AdventOfCode2021

open System

module Day7 =

    let setup (line:string) =
        line.Split([|","|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int

    let puzzle1 line =
        line 
        |> setup
        |> fun s -> Seq.min s, Seq.max s, s
        |||> fun min max s -> seq{min..max} |> Seq.map (fun v -> (0,s) ||> Seq.fold (fun sum v1 -> sum + abs(v1 - v))) |> Seq.min

    let puzzle2 line =
        line 
        |> setup
        |> fun s -> Seq.min s, Seq.max s, s
        |||> fun min max s -> 
            seq{min..max}
            |> Seq.map (fun v -> (0,s) ||> Seq.fold (fun sum v1 -> sum + ((seq{1..abs(v1 - v)}|> Seq.sum)))) 
        |> Seq.min
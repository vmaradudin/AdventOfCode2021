namespace AdventOfCode2021

open System.Text.RegularExpressions
open Common.Types

module Day02 =
    type Movement =
        | Up of int
        | Down of int
        | Forward of int

    let operationRegex = Regex("(?<op>forward|up|down)[\s]?(?<val>\d+)?")

    let map v =
        operationRegex.Match(v) 
        |> fun m -> m.Groups.["op"].Value, (m.Groups.["val"].Value |> int)
        |> fun a ->
            match a with
            | o, v when o = "forward" -> Forward v
            | o, v when o = "up" -> Up v
            | o, v when o = "down" -> Down v

    let puzzle1 seq =
        seq 
        |> Array.map map 
        |> fun s -> ((0,0), s) 
        ||> Seq.fold (fun (x,y) mov ->
            match mov with
            | Up u -> x, y  - u
            | Down d -> x, y + d
            | Forward f -> x + f, y)
        ||> fun x y -> x * y

    let puzzle2 seq =
        seq
        |> Array.map map
        |> fun s -> ((0,0,0), s)
        ||> Seq.fold(fun (x,y,a) mov ->
            match mov with
            |Down d -> x, y, a + d
            |Up u -> x, y, a - u
            |Forward f -> x + f, y + f * a, a)
        |||> fun x y _ -> x * y

    let Solution = (new Solution(2, puzzle1, puzzle2) :> ISolution).Execute
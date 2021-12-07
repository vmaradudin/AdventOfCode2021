namespace AdventOfCode2021

open System.Text.RegularExpressions

module Day5 =

    let coordRegex = Regex("(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)")

    let getSection line =
       coordRegex.Match(line)
       |> fun m -> (((m.Groups.["x1"].Value |> int),(m.Groups.["y1"].Value |> int)),
                    ((m.Groups.["x2"].Value |> int),(m.Groups.["y2"].Value |> int)))
       |> fun ((x1,y1),(x2,y2)) ->
        ((x1,y1),(x2,y2)),
        (fun x -> (x2*y1 - x1*y2 - (y1-y2)*x)/(x2-x1))

    let getSections lines =
        lines
        |> Array.map getSection

    let getCoordinates (((x1,y1),(x2,y2)),getY)=
        match ((x1,y1),(x2,y2)) with
        | ((x1,y1),(x2,y2)) when x1=x2 -> seq {for y in (min y1 y2)..(max y1 y2) -> (x1, y)}
        | ((x1,y1),(x2,y2)) when y1=y2 -> seq {for x in (min x1 x2)..(max x1 x2) -> (x, y1)}
        | ((x1,_),(x2,_)) -> seq {for x in (min x1 x2)..(max x1 x2) -> (x, (getY x))}

    let countIntersections state =
        (Seq.empty, state)
        ||> Array.fold(fun acc coord -> Seq.append acc (getCoordinates coord))
        |> Seq.countBy id
        |> Seq.filter (fun ((_,_),count) -> count>1)
        |> Seq.length

    let puzzle1 lines=
        lines
        |> getSections
        |> Array.filter (fun (((x1,y1),(x2,y2)),_) -> (x1=x2 ||y1=y2))
        |> countIntersections

    let puzzle2 lines=
        lines 
        |> getSections
        |> countIntersections
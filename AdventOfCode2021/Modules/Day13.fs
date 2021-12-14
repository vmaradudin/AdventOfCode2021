namespace AdventOfCode2021

open System
open Common.Tools
open Common.Types

module Day13 =

    let setup (lines:string[]) =
        let state = 
            lines
            |> Seq.takeWhile (fun l -> l|> String.IsNullOrWhiteSpace |>not)
            |> Seq.map (fun c -> 
                c.Split(",", StringSplitOptions.RemoveEmptyEntries)
                |> fun a -> 
                    ((a|>Seq.head |> int), (a|>Seq.last|> int)))
        let instructions=
            lines
            |> Seq.skip ((state |> Seq.length) + 1)
            |> Seq.map (fun s -> s.Replace("fold along ","").Split("=",StringSplitOptions.RemoveEmptyEntries) |> fun a -> (a.[0], (a.[1]|>int)))
            |> Seq.map(function
               | ("x",x) -> (x, 0)
               | ("y",y) -> (0, y))
        (state, instructions)

    let folder (array:seq<int*int>) (x,y)=
        if y>0 then
            array|> Seq.groupBy(fun (i,j) -> if j<=y then j else 2*y-j) |> Seq.map (fun (j, init) -> init|>Seq.map(fun (a,b) -> (a,j))) |> Seq.collect id|> Seq.distinct
        else
            array|> Seq.groupBy(fun (i,j) -> if i<=x then i else 2*x-i) |> Seq.map (fun (i, init) -> init|>Seq.map(fun (a,b) -> (i,b))) |> Seq.collect id|> Seq.distinct

    let puzzle1 input = 
            input
            |> setup
            ||> fun array instructions -> (array, instructions|>Seq.head)
            ||> folder
            |> Seq.length
            |> string

    let puzzle2 input =
            input
            |> setup
            ||> fun array instructions -> instructions|> Seq.fold folder array
            |> fun result -> result, (result|> Seq.maxBy fst|> fst, result|> Seq.maxBy snd|> snd)
            ||> fun result (maxX, maxY) ->
                [|for j in 0..maxY -> [|for i in 0..maxX -> if result |> Seq.contains (i,j) then '#' else ' '|] |> String.Concat|]
            |> fun displayLines -> Environment.NewLine + String.Join(Environment.NewLine, displayLines)

    let Solution = new Solution(13, puzzle1, puzzle2) 
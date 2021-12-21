namespace AdventOfCode2021

open System
open Common.Types

module Day14 =

    let setup (lines:string[]) =
        let state = 
            lines
            |> Seq.head
        let map = 
            lines
            |> Array.skip (2)
            |> Seq.map (fun s -> s.Split(" -> ")|> fun a ->  (a|>Array.head , (a|>Array.head|> fun b -> b.Insert(1, a|>Seq.last) |> Seq.windowed 2 |> Seq.map String.Concat)))
            |> Map.ofSeq
        (state, map)

    let groupSum (acc:seq<string*int64>) =
        acc
        |> Seq.groupBy fst 
        |> Seq.map (fun (s, seq) -> (s, seq|> Seq.sumBy snd))

    let rec project (acc:seq<string*int64>) (map:Map<string,seq<string>>) deep lastSymbol =
        if deep =0 then
            acc
            |> Seq.map( fun (a,v) -> (a.Substring(0,1), v))
            |> groupSum
            |> Seq.map (fun (s, c) -> if (s |> char = lastSymbol) then (s, c + 1L) else (s,c))
            |> fun a -> ((a|> Seq.maxBy snd|> snd) - (a|> Seq.minBy snd |> snd))                
        else
            acc 
            |> Seq.map(fun (s, count) -> map.[s] |> Seq.map( fun v -> (v, count)))
            |> Seq.collect id
            |> groupSum
            |> fun a -> project a map (deep - 1) lastSymbol

    let run (line:string) map stepCount =
        (line, line|> Seq.last)
        ||> fun line lastSymbol -> project (line|> Seq.windowed 2 |> Seq.map (fun a -> (a|>String.Concat, 1L))) map stepCount lastSymbol

    let puzzle1 input = 
        input
        |> setup
        ||> fun line map -> run line map 10
    
    let puzzle2 input = 
       input
       |> setup
       ||> fun line map -> run line map 40

    let Solution = (new Solution(14, puzzle1, puzzle2) :> ISolution).Execute
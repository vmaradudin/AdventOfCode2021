namespace AdventOfCode2021

open System
open Common.Tools
open Common.Types

module Day12 =

    let setup (lines:string[]) =
        lines
        |> Seq.map (fun c -> c.Split("-", StringSplitOptions.RemoveEmptyEntries)|> fun a -> ((a|>Seq.head), (a|>Seq.last)))
        |> Seq.map (fun (s,e) -> [(e,s);(s,e)])
        |> Seq.collect id
        |> Seq.filter (fun (s,e) -> e <> "start" && s<>"end")

    let computePathStep existingChain caveMap smallCaveCondition=
        let lastNode = 
            existingChain
            |>Seq.last
        if lastNode = "end" then 
            seq{existingChain |> String.concat ","}
        else
            let visitedSmallCaves = 
                existingChain
                |> Seq.filter(fun a -> a = a.ToLower()) 
                |> Seq.countBy id

            let possibleNextMovements = 
                caveMap
                |> Seq.filter (fun (s,_) -> s = lastNode)
                |> Seq.map snd
                |> Seq.filter (smallCaveCondition visitedSmallCaves)

            if possibleNextMovements |> Seq.isEmpty then
                Seq.empty
            else
                possibleNextMovements
                |> Seq.map(fun a -> ([a]|> Seq.append existingChain)|> String.concat ",")
        

    let rec findPath existingChains caveMap smallCaveCondition=
        if existingChains |> Seq.forall(fun a -> (a|>Seq.last) = "end") then
            existingChains
        else
            let currentItems = 
                existingChains
                |> Seq.map(fun a -> computePathStep a caveMap smallCaveCondition)
                |> Seq.collect id
                |> Seq.distinct
                |> Seq.map(fun a -> a.Split(",", StringSplitOptions.RemoveEmptyEntries)|> Seq.ofArray)
            findPath currentItems caveMap smallCaveCondition

    let pathCount input smallCaveCondition =
        input
        |> setup
        |> fun caveMap -> (caveMap, smallCaveCondition)
        ||> findPath (Seq.singleton (Seq.singleton "start")) 
        |> Seq.length

    let puzzle1 input =
        let smallCaveCondition caves cave = caves |> Seq.map fst|> Seq.contains(cave) |> not
        pathCount input smallCaveCondition


    let puzzle2 input =
        let smallCaveCondition (caves:seq<string*int>) (cave:string) = 
            caves
            |> match (Seq.exists (fun (_,c) -> c=2) caves) with
                | true -> fun a -> a|>Seq.map fst|> Seq.contains(cave) |> not
                | false -> fun _ -> true
        pathCount input smallCaveCondition
    
    let Solution = new Solution(12, puzzle1, puzzle2) 
namespace AdventOfCode2021

open Common.Tools
open Common.Types

module Day11 =

    let setup (lines:string[]) =
        lines
         |> Seq.map (Seq.map(fun c -> c.ToString() |> int)) 
         |> array2D

    let getFlashingAdjacentsCount point array=
        array
        |> getAdjacentCoordinates point
        |> Seq.filter (fun (i,j) -> array.[i,j] = 10)
        |> Seq.length
    
    let rec step array start=
        let transformedArray = 
            array 
            |> Array2D.mapi (fun i j v -> 
                match start with 
                |true -> v + 1
                |_-> match v with
                     | 10 -> 11
                     | a when a < 10->
                        match (v, v + (getFlashingAdjacentsCount (i,j) array)) with 
                            | (oldValue,newValue) when oldValue<10 && newValue>=10 -> 10
                            | (_, newValue) -> newValue
                     |_ -> v)

        transformedArray
        |> flatten
        |> Seq.filter ((=)10)
        |> Seq.length
        |> function 
            | 0 -> transformedArray |> Array2D.map(fun v -> if v >=10 then 0 else v)
            | _ -> step transformedArray false
    
    let rec solution1 acc currentStep stepCount array= 
        let stepResult = step array true
        let flashedCount =
            stepResult
            |> flatten
            |> Seq.filter ((=)0)
            |> Seq.length
        match currentStep with
        | v when v = (stepCount - 1)-> acc + flashedCount
        | _ -> solution1 (acc + flashedCount) (currentStep + 1) stepCount stepResult

    let rec solution2 currentStep array= 
        let stepResult = step array true
        match (stepResult |> flatten |> Seq.forall((=)0)) with
        | true -> currentStep + 1
        | false -> solution2 (currentStep+1) stepResult

    let puzzle1 input =
        input
        |> setup
        |> solution1 0 0 100

    let puzzle2 input =
        input
        |> setup
        |> solution2 0
    
    let Solution = (new Solution(11, puzzle1, puzzle2)  :> ISolution).Execute
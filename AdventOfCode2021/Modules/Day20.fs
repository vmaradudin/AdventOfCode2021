namespace AdventOfCode2021

open System
open Common.Types

module Day20 =

    let binToDec b =
        (b, (0,1))
        ||> Seq.foldBack(fun c (d, p) -> d + (c|>string|>int)*p, p*2)
        |> fst

    let rec setup (input:string[])= 
        let algorithm = input.[0]
        let inputImage = input.[2..]
        (algorithm, inputImage)
            
    let getValue (inputImage:string[]) (x,y) defaultSymbol =
        if x<0 || y<0 || x>=inputImage.[0].Length || y>=inputImage.Length then 
            defaultSymbol
        else inputImage.[y].[x]

    let getCurrentValue inputImage defaultValue (x,y) =
        [| for j in y-1..y+1 do
            for i in x-1..x+1 do
                (getValue inputImage (i,j) defaultValue)
        |]
        |> String.Concat
        |> fun s -> s.Replace('.','0').Replace('#','1')

          
    let trimImage defaultSymbol (inputImage:string[]) =
        let nonDefaultSymbol = if defaultSymbol = '.' then '#' else '.'
        let newImage = 
            inputImage
            |> Array.skipWhile( fun s -> not(s.Contains(nonDefaultSymbol)))
            |> Array.rev
            |> Array.skipWhile( fun s -> not(s.Contains(nonDefaultSymbol)))
            |> Array.rev
        
        let imageBorders=
            newImage
            |> Array.map(fun s -> (match s.IndexOf(nonDefaultSymbol) with | -1 -> None |v -> Some(v)), (match s.LastIndexOf(nonDefaultSymbol) with | -1 -> None |v -> Some(v)))
        let leftBorder = imageBorders |> Array.map fst |> Array.choose id |> Array.min
        let rightBorder = imageBorders |> Array.map snd |> Array.choose id |> Array.max
        newImage
        |> Array.map(fun s -> s.Remove(rightBorder+1).Substring(leftBorder))

    let calculateSymbol (algorithm:string) (inputImage:string[]) defaultSymbol =
        [|for j in -3..(inputImage.Length)+2 ->
            [|for i in -3..(inputImage.[0].Length)+2 ->
                (i,j)
                |> getCurrentValue inputImage defaultSymbol
                |> binToDec
                |> fun position -> algorithm.[position]|]
            |> String.Concat
                |]
        |> trimImage defaultSymbol

    let rec run (algorithm:string) (inputImage:string[]) count defaultSymbol =
        if count = 0 then
            inputImage
        else
            let newImage = calculateSymbol algorithm inputImage defaultSymbol
            let newDefaultSymbol = if defaultSymbol = '.' then algorithm.[0] else algorithm.[511]
            run algorithm newImage (count-1) newDefaultSymbol

    let puzzle1 (input) = 
        input
        |> setup
        |> fun (alg,img) -> run alg img 2 '.'
        |> String.Concat
        |> fun s -> s.Replace(".","").Length
     
    let puzzle2 input = 
        input
        |> setup
        |> fun (alg,img) -> run alg img 50 '.'
        |> String.Concat
        |> fun s -> s.Replace(".","").Length
        
    let Solution = (new Solution(20, puzzle1, puzzle2) :> ISolution).Execute
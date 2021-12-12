namespace AdventOfCode2021

open System

module Day08 =

    let setup (lines:string[]) =
        lines|> 
        Array.map (fun s -> s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries))
    
    let puzzle1 lines =
        lines 
        |> setup
        |> Array.map (fun a-> 
            a
            |> Array.skip 11
            |> Array.countBy String.length 
            |> Array.where(fun (length, count) -> length =2 || length =3 || length=4 || length=7) 
            |> Array.sumBy snd)
        |> Array.sum
    
    // Stats for working screen
    // a - 8
    // b - 6
    // c - 8
    // d - 7
    // e - 4
    // f - 9
    // g - 7

    let getKey (entry:string[]) =
        // collect stats for broken screen
        let brokenStats =
            entry 
            |> Array.take 10 
            |> String.Concat 
            |> Seq.map id 
            |> Seq.countBy id
        
        // get known values
        let digit1 = entry |> Array.find(fun a -> a.Length=2)
        let digit4 = entry |> Array.find(fun a -> a.Length=4)

        // find matches for statistics with usage of known values where there is ambiguity (a,c and d,g)
        let brokenF = brokenStats |> Seq.find (fun (_, s) -> s=9) |> fst
        let brokenC = brokenStats |> Seq.find (fun(symbol, s) -> s=8 && digit1.Contains(symbol)) |> fst
        let brokenA = brokenStats |> Seq.find (fun(symbol, s) -> s=8 && symbol <> brokenC) |> fst
        let brokenB = brokenStats |> Seq.find (fun (_, s) -> s=6) |> fst
        let brokenE = brokenStats |> Seq.find (fun (_, s) -> s=4) |> fst
        let brokenG = brokenStats |> Seq.find (fun(symbol, s) -> s=7 && not(digit4.Contains(symbol))) |> fst
        let brokenD = brokenStats |> Seq.find (fun (symbol, s) -> s=7 && symbol <> brokenG) |> fst

        // return maping
        seq{(brokenA, 'a'); (brokenB, 'b'); (brokenC, 'c'); (brokenD, 'd'); (brokenE, 'e'); (brokenF, 'f'); (brokenG, 'g')}

    // apply decoding mapping
    let decode (key:seq<char*char>) value =
        value
        |> Seq.map(fun e -> key |> Seq.find (fun (k,v) -> k=e) |> snd)
        |> Seq.sort
        |> String.Concat

    // translate into digits
    let decodeDigit (entry:string)=
        entry
        |> Seq.sort
        |> String.Concat
        |> fun o ->
            match o with
            | "abcefg"-> '0'
            | "cf" -> '1'
            | "acdeg" -> '2'
            | "acdfg"-> '3'
            | "bcdf"-> '4'
            | "abdfg"-> '5'
            | "abdefg" -> '6'
            | "acf"-> '7'
            | "abcdefg" -> '8'
            | "abcdfg" -> '9'

    let puzzle2 line =
        line 
        |> setup
        |> Array.map(fun a -> 
            ((a|>Array.skip 11), (a|>getKey))
            ||> fun output key -> output |> Array.map (decode key)
            |> Array.map decodeDigit |> String.Concat)
        |> Seq.map int
        |> Seq.sum
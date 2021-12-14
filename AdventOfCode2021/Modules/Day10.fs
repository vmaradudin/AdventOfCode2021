namespace AdventOfCode2021

open System
open Common.Types

module Day10 =

    let setup (lines:string[]) =
        lines
        |> Seq.ofArray

    let rec checkLine (acc: char list) line=
        let s = line|> Seq.tryHead
        match s with
        | None when acc|>Seq.isEmpty -> None
        | None when acc|>Seq.exists (fun _ -> true) -> Some("I"+ String.Concat(acc |> Seq.ofList))
        | Some(symbol) ->
            match symbol with
            | '(' | '[' | '{' | '<' -> checkLine (symbol::acc) (line|>Seq.tail)
            | '>' | '}' | ']' | ')' -> 
                match acc with
                |c when c|> Seq.isEmpty -> None
                |c ->
                    match (symbol, c|>Seq.head) with
                    |('>','<') | ('}','{') | (']','[') | (')','(') -> checkLine (acc |> List.tail) (line|>Seq.tail)
                    |_-> Some(symbol |> string)

    let puzzle1 input =
        input
        |> setup
        |> Seq.map (fun s -> checkLine List.empty s)
        |> Seq.filter (function Some _ -> true |_-> false)
        |> Seq.map(function
            |Some ">" -> 25137
            |Some "}" -> 1197
            |Some "]" -> 57
            |Some ")" -> 3
            |_ -> 0 )
        |> Seq.sum

    let puzzle2 input =
        input
        |> setup
        |> Seq.map (fun s -> checkLine List.empty s)
        |> Seq.filter (function Some v when v.StartsWith "I" -> true |_-> false)
        |> Seq.map (function Some s -> (s|> Seq.tail|>String.Concat) | _ -> "")
        |> Array.ofSeq
        |> Array.map(fun s -> s|> Seq.map(function 
            | '('->1L
            | '[' -> 2L 
            | '{' -> 3L
            | '<' -> 4L
            | _ -> 0L))
        |> Array.map (fun a -> a|>Seq.fold (fun acc v -> acc*5L+v) 0L)
        |> Array.sort
        |> fun a -> a.[a.Length/2]
    
    let Solution = new Solution(10, puzzle1, puzzle2) 
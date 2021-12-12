namespace AdventOfCode2021

open System

module Day06 =

    let setupDay0 (line:string) =
        line.Split([|","|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int
        |> Seq.countBy id
        |> fun s -> Seq.init 9 (fun i -> match (Seq.tryFind(fun (age,_) -> age = i) s) with
                                            |Some(v) -> (int64)(snd v)
                                            |None -> 0L)

    let rec liveDay state day endOfLife =
        state
        |> fun s -> (s|>Seq.tail), Seq.singleton (Seq.head s)
        ||> Seq.append
        |> fun s -> s|>Seq.updateAt 6 ((Seq.item 6 s) + (Seq.last s))
        |> fun s -> s, day + 1
        ||> fun s nextDay ->
            match nextDay with
            | x when x = endOfLife -> s
            | _ -> liveDay s nextDay endOfLife

    let puzzle1 line =
        line 
        |> setupDay0
        |> fun s -> liveDay s 0 80
        |> Seq.sum

    let puzzle2 line =
        line 
        |> setupDay0
        |> fun s -> liveDay s 0 256
        |> Seq.sum
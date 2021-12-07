namespace AdventOfCode2021

module Day3 =
    let toInt c =
        if c ='1' then 1 else 0
    
    let puzzle1 set =
        let totalCount = set |> Seq.length
        set 
        |> Seq.map (fun s -> s|>Seq.map toInt)
        |> Seq.transpose
        |> Seq.map Seq.sum
        |> Seq.map (fun c -> if 2*c > totalCount then 1 else 0)
        |> fun r -> r, (0, 0, 1) 
        ||> Seq.foldBack (fun v (a, b, p) -> a + v*p, b + abs(v - 1)*p, p*2) 
        |||> fun a b _ -> a*b

    
    let rec calcAndAppend sequence acc oxygen =
        sequence
        |> Seq.groupBy Seq.head
        |> Seq.map (fun (key, s) -> key, Seq.length s, Seq.map Seq.tail s)
        |> fun s -> 
            let avg = s |> Seq.averageBy (fun (_, b, _) -> (b|>float))
            if (s |> Seq.forall(fun (_, b, _) -> (b |> float) = avg))
                then (s |> Seq.find(fun (a,_,_) -> a=oxygen))
                else
                if oxygen = 1 
                    then s|> Seq.maxBy (fun (_, l, _) -> l)
                    else s|> Seq.minBy(fun (_,l,_) -> l)
        |||> fun key length t -> 
            if length > 1 
            then calcAndAppend t (acc + (key|>string)) oxygen
            else  (acc + (key|>string), (t |>Seq.exactlyOne)) ||> Seq.fold(fun acc v -> acc + (v|>string)) 

    let binToDec (s:string) = 
        (s, (0,1))
        ||> Seq.foldBack(fun c (d, p) -> d + (c|>toInt)*p, p*2)
        ||> fun a _ -> a

    let puzzle2 set =
        set 
        |> Seq.map (fun s -> s|>Seq.map toInt)
        |> fun a -> (calcAndAppend a "" 1), (calcAndAppend a "" 0)
        ||> fun oxy co2 -> (oxy|>binToDec) * (co2|>binToDec)
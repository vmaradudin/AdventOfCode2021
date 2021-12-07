namespace AdventOfCode2021

open System

module Day4 =
    let setupGame (lines:string[]) =
        lines
        |> fun file -> Array.head file, Array.tail file
        ||> fun gameLine cardLines ->  
            (gameLine.Split([|","|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s|> int)), 
            cardLines 
            |> Array.filter (fun a -> (not(String.IsNullOrWhiteSpace(a))))
            |> Array.map(fun v -> v.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun a -> Some(a|>int))|> Seq.ofArray)
            |> Seq.ofArray
            |> fun a -> Seq.splitInto((a|>Seq.length)/5) a
            |> Seq.map Seq.ofArray |> Seq.map array2D |> Array.ofSeq |> Array.map (fun v -> v, false)
    
    let checkCard (card:int option[,]) i j =
        (card.[i,*], card[*,j])
        ||> fun row column ->  (card, (Array.forall(fun v -> v = None) row) || (Array.forall(fun v-> v = None) column))

    let playCard (card:int option[,]*bool) value =
        let mutable coord = None
        card
        ||> fun crd won -> 
            (crd |> Array2D.mapi(fun i j v ->
            match v with
            | Some(x) when x = value -> 
                coord <- Some(i,j)
                None
            | _ -> v), won)
        ||> fun crd won ->
            match coord with
            | Some(i,j) when not(won) -> checkCard crd i j
            | _ -> (crd, won)
            

    let rec play game (cards:seq<int option[,]*bool>) stopCondition =
        let step = game |> Array.head
        cards
        |> Seq.map (fun card -> 
            let newCard = playCard card step
            newCard, not (card|>snd) && (newCard|>snd))
        |> fun playedCards -> 
        if stopCondition playedCards
            then playedCards, step
            else play (game|> Array.tail) (playedCards|>Seq.map fst) stopCondition

        
    let calculateSum (card:int option[,]) =
        seq {for x in 0..(Array2D.length1 card - 1) -> card[x,*]|>Array.sumBy(fun v -> match v with |Some(a) -> a |_->0)}
        |> fun a -> a|>Seq.sum

    let puzzle1 (lines:string[])=
        lines 
        |> setupGame 
        ||> fun game cards -> (game, cards, (fun b -> b|> Seq.filter (fun ((card,winner), newWinner) -> winner) |> Seq.length = 1))
        |||> play 
        ||> fun cards step -> (cards |> Seq.find (fun c -> c |> fst |>snd) |> fst |> fst), step
        ||> fun winnerCard step -> (calculateSum winnerCard) * step

    let puzzle2 (lines:string[] )=
        lines 
        |> setupGame 
        ||> fun game cards -> (game, cards, (fun b -> b|> Seq.forall (fun ((card,winner), newWinner) -> winner)))
        |||> play 
        ||> fun cards step -> (cards |> Seq.find (fun c -> c |> snd) |> fst |> fst), step
        ||> fun winnerCard step -> (calculateSum winnerCard) * step
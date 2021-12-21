namespace AdventOfCode2021

open System
open Common.Types

module Day21 =

    type Player =
        struct
            val Position: int
            val Score: int
            new(position: int, score:int) = { Position = position; Score = score; }
            member x.Play (step : int) =
                       let nextPosition = (x.Position + step) % 10 |> function |0 -> 10 |v -> v
                       new Player(nextPosition, x.Score + nextPosition )
        end
        with override x.ToString()=
              x.Position.ToString() + ", "+ x.Score.ToString()

    type DeterministicDice =
        struct
            val Rolled: int
            val Number: int
            new (rollCount:int) = { Rolled = rollCount; Number = 0 }
            new (rollCount:int, number:int) = { Rolled = rollCount; Number = number }
            member x.Roll () =
                let newRolled = x.Rolled + 1
                let newNumber = newRolled % 100 |> function |0 -> 100 |v -> v
                new DeterministicDice(newRolled, newNumber)
        end

    let setup (input:string[]) =
        input
        |> Array.map (fun s -> s.Substring(s.LastIndexOf(' ') + 1) |> int)
        |> Array.map (fun position -> new Player(position, 0))
        |> fun p -> p.[0], p.[1]

    let roll (times:int) (dice:DeterministicDice) =
        let diceRolled = dice.Rolled + times
        dice 
        |> Array.unfold (fun d -> if d.Rolled >= diceRolled  then None else Some(d.Roll(),d.Roll()))
        |> fun a -> a|> Array.sumBy (fun d -> d.Number) , a |> Array.last

    let playTurn (dice:DeterministicDice) (player:Player) =
        let diceResults = dice |> roll 3 
        let points = fst diceResults |> int
        let newDiceState = snd diceResults
        newDiceState, player.Play(points)
    
    let checkWin (winScore:int) (player:Player) =
        player.Score >= winScore

    let rec playGame (dice:DeterministicDice) winCondition (player:Player) (otherPlayer:Player)=
            match otherPlayer with
            | p when winCondition p -> 
                (player.Score)*(dice.Rolled)
            |_ ->
                player
                |> playTurn dice
                |> fun (newDice, playerDone) ->
                    playGame newDice winCondition otherPlayer playerDone


    let quantumDieOptions =
        ([|1..3|],[|1..3|])
        ||> Array.allPairs  
        |> Array.allPairs [|1..3|]
        |> Array.map(fun (a,(b,c)) -> a+b+c)
        |> Array.countBy id

    let merge array key valueSelector=
        array
        |> Array.groupBy key
        |> Array.map(fun (k, v) -> k, (v|> valueSelector))

    let getUniversesResults (player:Player) =
            quantumDieOptions 
            |> Array.map (fun (points, universes) -> (player.Play(points), universes))
            |> Array.groupBy fst 
            |> Array.map(fun (player, values) -> player, (values|> Array.unzip |> snd |> Array.sum |> int64))

    let collectWins (universe:((Player*Player)*int64)[]) =
            universe
            |> Array.map (fun ((player1, player2), universeCount) -> 
                   match ((player1, player2), universeCount) with
                    | ((p1, p2), uc) when p1.Score >= 21 && p2.Score < 21 -> (uc, 0L)
                    | ((p1, p2), uc) when p1.Score < 21 && p2.Score >= 21 -> (0L, uc)
                    |_ -> (0L,0L))
            |> Array.unzip
            ||> fun w1 w2 -> (w1 |> Array.sum), (w2 |>Array.sum)
        
    let makeStep (player1:Player) (universe:((Player*Player)*int64)[]) =
        let array = player1 |> getUniversesResults
        let array2 = universe |> Array.map(fun ((_,p2),count) -> (p2,count))
        Array.allPairs array array2
        |> Array.map (fun ((p1, c1),(p2,c2)) -> ((p1,p2), c1*c2))
        |> Array.groupBy (fun (state,_) -> state)
        |> Array.map(fun (state, stateWithUniverseNumber) -> state, stateWithUniverseNumber|>Array.sumBy snd)

    let rec playGame2 (universe:((Player*Player)*int64)[]) wins1 wins2=
        if (universe.Length = 0) then
            (wins1, wins2)
        else
        let universeAfterStep =
            universe
            |> Array.groupBy(fun ((p1,_),_) -> p1)
            |> Array.map (fun (p,universes) -> makeStep p universes)
            |> Array.collect id
            |> Array.groupBy fst
            |> Array.map(fun (state, cnt) -> (state, (cnt|>Array.sumBy snd)))
        
        let wins = 
            collectWins universeAfterStep
            ||> fun w1 w2 -> (wins1 + w1), (wins2 + w2)

        let universeReadyForNextStep =
            universeAfterStep
            |> Array.filter(fun ((player1,player2), _) -> player1.Score < 21 && player2.Score < 21)
            |> Array.unzip
            ||> fun players universes -> 
                players 
                |> Array.unzip 
                ||> fun player1 player2 -> 
                    Array.zip player2 player1
                    |> fun p -> (p, universes)
                    ||> Array.zip

        playGame2 universeReadyForNextStep (snd wins) (fst wins)
                
    let puzzle1 (input) = 
        input
        |> setup
        ||> playGame (DeterministicDice()) (checkWin 1000)
     
    let puzzle2 input = 
        input
        |> setup
        ||> fun player1 player2 ->  playGame2 [|((player1,player2),1L)|] 0L 0L
        ||> max
        
    let Solution = ((new Solution(21, puzzle1, puzzle2)) :> ISolution).Execute
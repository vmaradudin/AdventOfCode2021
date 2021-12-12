namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day02

module Day02Tests =

    let testInput = [|"forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2"|] |> Seq.ofArray
    
    [<Fact>]
    let ``Day 2 Puzzle 1`` () =
        Assert.Equal(150, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 2 Puzzle 2`` () =
        Assert.Equal(900, puzzle2 testInput)      
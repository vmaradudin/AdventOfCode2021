namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day03

module Day03Tests =

    let testInput = [|"00100"; "11110"; "10110"; "10111"; "10101"; "01111"; "00111"; "11100"; "10000"; "11001"; "00010"; "01010"|] |> Seq.ofArray
    
    [<Fact>]
    let ``Day 3 Puzzle 1`` () =
        Assert.Equal(198, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 3 Puzzle 2`` () =
        Assert.Equal(230, puzzle2 testInput)      
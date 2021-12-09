namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day9

module Day9Tests =

    let testInput = [|
        "2199943210"
        "3987894921"
        "9856789892"
        "8767896789"
        "9899965678"
        |]
    
    [<Fact>]
    let ``Day 9 Puzzle 1`` () =
        Assert.Equal(15, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 9 Puzzle 2`` () =
        Assert.Equal(1134, puzzle2 testInput)
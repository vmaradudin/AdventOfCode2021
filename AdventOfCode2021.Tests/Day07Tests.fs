namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day07

module Day07Tests =

    let testInput = "16,1,2,0,4,2,7,1,2,14"
    
    [<Fact>]
    let ``Day 7 Puzzle 1`` () =
        Assert.Equal(37, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 7 Puzzle 2`` () =
        Assert.Equal(168, puzzle2 testInput)      
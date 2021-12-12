namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day06

module Day06Tests =

    let testInput = "3,4,3,1,2"
    
    [<Fact>]
    let ``Day 6 Puzzle 1`` () =
        Assert.Equal(5934L, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 6 Puzzle 2`` () =
        Assert.Equal(26984457539L, puzzle2 testInput)      
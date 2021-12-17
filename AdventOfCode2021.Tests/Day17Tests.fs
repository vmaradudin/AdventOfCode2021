namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day17

module Day17Tests =
    
    let testInput ="target area: x=20..30, y=-10..-5"
    
    [<Fact>]
    let ``Day 17 Puzzle 1`` () =
        Assert.Equal(45, puzzle1 [|testInput|])
    
    [<Fact>]
    let ``Day 17 Puzzle 2`` () =
        Assert.Equal(112, puzzle2 [|testInput|])
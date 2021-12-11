namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day11

module Day11Tests =

    let testInput = [|
        "5483143223"
        "2745854711"
        "5264556173"
        "6141336146"
        "6357385478"
        "4167524645"
        "2176841721"
        "6882881134"
        "4846848554"
        "5283751526"
        |]
    
    [<Fact>]
    let ``Day 11 Puzzle 1`` () =
        Assert.Equal(1656, puzzle1 testInput)
        Assert.Equal(204, solution1 0 0 10 (testInput|>setup))
    
    [<Fact>]
    let ``Day 11 Puzzle 2`` () =
        Assert.Equal(195, puzzle2 testInput)
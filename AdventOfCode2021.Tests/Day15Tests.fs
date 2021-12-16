namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day15

module Day15Tests =

    let testInput = [|
        "1163751742"
        "1381373672"
        "2136511328"
        "3694931569"
        "7463417111"
        "1319128137"
        "1359912421"
        "3125421639"
        "1293138521"
        "2311944581"
        |]
    
    [<Fact>]
    let ``Day 15 Puzzle 1`` () =
        Assert.Equal(40, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 15 Puzzle 2`` () =
        Assert.Equal(315, puzzle2 testInput)
        
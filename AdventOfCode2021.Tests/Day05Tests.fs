namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day05

module Day05Tests =

    let testInput = [|"0,9 -> 5,9";"8,0 -> 0,8";"9,4 -> 3,4";"2,2 -> 2,1";"7,0 -> 7,4";"6,4 -> 2,0";"0,9 -> 2,9";"3,4 -> 1,4";"0,0 -> 8,8";"5,5 -> 8,2"|]
    
    [<Fact>]
    let ``Day 5 Puzzle 1`` () =
        Assert.Equal(5, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 5 Puzzle 2`` () =
        Assert.Equal(12, puzzle2 testInput)      
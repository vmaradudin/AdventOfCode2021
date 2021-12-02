namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day1

module Day1Tests =
    let testInput = 
        [|
          199
          200
          208
          210
          200
          207
          240
          269
          260
          263
        |]  
    
    [<Fact>]
    let ``Day 1 Puzzle 1`` () =
        Assert.Equal(7, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 1 Puzzle 2`` () =
        Assert.Equal(5, puzzle2 testInput)      
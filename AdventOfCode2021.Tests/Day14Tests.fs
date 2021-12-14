namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day14

module Day14Tests =

    let testInput = [|
        "NNCB"
        ""
        "CH -> B"
        "HH -> N"
        "CB -> H"
        "NH -> C"
        "HB -> C"
        "HC -> B"
        "HN -> C"
        "NN -> C"
        "BH -> H"
        "NC -> B"
        "NB -> B"
        "BN -> B"
        "BB -> N"
        "BC -> B"
        "CC -> N"
        "CN -> C"
        |]
    
    [<Fact>]
    let ``Day 14 Puzzle 1`` () =
        Assert.Equal(1588L, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 14 Puzzle 2`` () =
        Assert.Equal(2188189693529L, puzzle2 testInput)
        
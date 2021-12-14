namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day13

module Day13Tests =

    let testInput = [|
        "6,10"
        "0,14"
        "9,10"
        "0,3"
        "10,4"
        "4,11"
        "6,0"
        "6,12"
        "4,1"
        "0,13"
        "10,12"
        "3,4"
        "3,0"
        "8,4"
        "1,10"
        "2,14"
        "8,10"
        "9,0"
        ""
        "fold along y=7"
        "fold along x=5"
        |]
    
    [<Fact>]
    let ``Day 13 Puzzle 1`` () =
        Assert.Equal("17", puzzle1 testInput)
        
    
    [<Fact>]
    let ``Day 13 Puzzle 2`` () =
        let expected ="
#####
#   #
#   #
#   #
#####"
        Assert.Equal(expected, puzzle2 testInput)
        
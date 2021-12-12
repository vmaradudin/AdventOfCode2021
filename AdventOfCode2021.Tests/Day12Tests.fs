namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day12

module Day12Tests =

    let testInput1 = [|
        "start-A"
        "start-b"
        "A-c"
        "A-b"
        "b-d"
        "A-end"
        "b-end"
        |]
    let testInput2 = [|
        "dc-end"
        "HN-start"
        "start-kj"
        "dc-start"
        "dc-HN"
        "LN-dc"
        "HN-end"
        "kj-sa"
        "kj-HN"
        "kj-dc"
        |]
    let testInput3 = [|
        "fs-end"
        "he-DX"
        "fs-he"
        "start-DX"
        "pj-DX"
        "end-zg"
        "zg-sl"
        "zg-pj"
        "pj-he"
        "RW-he"
        "fs-DX"
        "pj-RW"
        "zg-RW"
        "start-pj"
        "he-WI"
        "zg-he"
        "pj-fs"
        "start-RW"
        |]

    
    [<Fact>]
    let ``Day 12 Puzzle 1`` () =
        Assert.Equal(10, puzzle1 testInput1)
        Assert.Equal(19, puzzle1 testInput2)
        Assert.Equal(226, puzzle1 testInput3)
    
    [<Fact>]
    let ``Day 12 Puzzle 2`` () =
        Assert.Equal(36, puzzle2 testInput1)
        Assert.Equal(103, puzzle2 testInput2)
        Assert.Equal(3509, puzzle2 testInput3)
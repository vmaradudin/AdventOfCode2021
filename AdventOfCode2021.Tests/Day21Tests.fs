namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day21

module Day21Tests=
    let testInput =
        [|
            "Player 1 starting position: 4"
            "Player 2 starting position: 8"
        |]

    [<Fact>]
    let ``Day 21 Puzzle 1`` () =
        Assert.Equal(739785, puzzle1 testInput)

    [<Fact>]
    let ``Day 21 Puzzle 2`` () =
        Assert.Equal(444356092776315L, puzzle2 testInput)
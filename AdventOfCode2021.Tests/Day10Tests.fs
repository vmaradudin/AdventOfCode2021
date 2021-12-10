namespace AdventOfCode2021.Tests

open Xunit
open AdventOfCode2021.Day10

module Day10Tests =

    let testInput = [|
        "[({(<(())[]>[[{[]{<()<>>"
        "[(()[<>])]({[<{<<[]>>("
        "{([(<{}[<>[]}>{[]{[(<()>"
        "(((({<>}<{<{<>}{[]{[]{}"
        "[[<[([]))<([[{}[[()]]]"
        "[{[{({}]{}}([{[{{{}}([]"
        "{<[[]]>}<{[{[{[]{()[[[]"
        "[<(<(<(<{}))><([]([]()"
        "<{([([[(<>()){}]>(<<{{"
        "<{([{{}}[<[[[<>{}]]]>[]]"
        |]
    
    [<Fact>]
    let ``Day 10 Puzzle 1`` () =
        Assert.Equal(26397L, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 10 Puzzle 2`` () =
        Assert.Equal(288957L, puzzle2 testInput)
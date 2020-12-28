namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay8 =

    [<Test>]
    let ``Test part 1`` () =
        Day8.part1 ()
        |> shouldEqual 2016

    [<Test>]
    let ``Test part 2`` () =
        Day8.part2 ()
        |> shouldEqual
            [
                "X  X XXXX  XX  XXXX X  X "
                "X  X    X X  X    X X  X "
                "XXXX   X  X      X  X  X "
                "X  X  X   X     X   X  X "
                "X  X X    X  X X    X  X "
                "X  X XXXX  XX  XXXX  XX  "
            ]
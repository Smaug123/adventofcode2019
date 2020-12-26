namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay3 =

    [<Test>]
    let ``Test part 1`` () =
        Day3.part1 ()
        |> shouldEqual 225

    [<Test>]
    let ``Test part 2`` () =
        Day3.part2 ()
        |> shouldEqual 35194

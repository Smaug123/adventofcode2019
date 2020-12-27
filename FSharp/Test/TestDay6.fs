namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay6 =

    [<Test>]
    let ``Test part 1`` () =
        Day6.part1 ()
        |> shouldEqual 249308

    [<Test>]
    let ``Test part 2`` () =
        Day6.part2 ()
        |> shouldEqual 349

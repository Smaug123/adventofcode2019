namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay4 =

    [<Test>]
    let ``Test part 1`` () =
        Day4.part1 ()
        |> shouldEqual 1855

    [<Test>]
    let ``Test part 2`` () =
        Day4.part2 ()
        |> shouldEqual 1253

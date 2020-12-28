namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay9 =

    [<Test>]
    let ``Test part 1`` () =
        Day9.part1 ()
        |> shouldEqual 2775723069L

    [<Test>]
    let ``Test part 2`` () =
        Day9.part2 ()
        |> shouldEqual 49115L

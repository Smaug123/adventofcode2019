namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay2 =

    [<Test>]
    let ``Test part 1`` () =
        Day2.part1 ()
        |> shouldEqual 3765464L

    [<Test>]
    let ``Test part 2`` () =
        Day2.part2 ()
        |> shouldEqual 7610L

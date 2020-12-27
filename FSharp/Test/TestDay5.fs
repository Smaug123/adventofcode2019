namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay5 =

    [<Test>]
    let ``Test part 1`` () =
        Day5.part1 ()
        |> shouldEqual 6731945

    [<Test>]
    let ``Test part 2`` () =
        Day5.part2 ()
        |> shouldEqual 9571668

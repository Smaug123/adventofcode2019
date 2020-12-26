namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay1 =

    [<Test>]
    let ``Test part 1`` () =
        Day1.part1 ()
        |> shouldEqual 3301059

    [<Test>]
    let ``Test totalFuel`` () =
        Day1.totalFuel 0 14
        |> shouldEqual 2
        Day1.totalFuel 0 1969
        |> shouldEqual 966
        Day1.totalFuel 0 100756
        |> shouldEqual 50346

    [<Test>]
    let ``Test part 2`` () =
        Day1.part2 ()
        |> shouldEqual 4948732

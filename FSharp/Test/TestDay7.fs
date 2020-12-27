namespace AdventOfCode.Test

open NUnit.Framework
open FsUnitTyped
open AdventOfCode

[<TestFixture>]
module TestDay7 =

    [<Test>]
    let ``Test permutations`` () =
        let perms = Day7.permutations [1 ; 2 ; 3 ; 4]
        perms.Length |> shouldEqual 24
        perms |> Set.ofList |> Set.count |> shouldEqual 24
        perms
        |> List.iter (fun i -> List.sort i |> shouldEqual [1 ; 2 ; 3 ; 4])

    [<Test>]
    let ``Test part 1`` () =
        Day7.part1 ()
        |> shouldEqual 255590

    [<Test>]
    let ``Test part 2`` () =
        Day7.part2 ()
        |> shouldEqual 58285150

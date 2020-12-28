namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day9 =

    let part1 () =
        Utils.readResource' "Day9Input.txt"
        |> IntCode.parse
        |> IntCode.run [1L]
        |> Seq.exactlyOne

    let part2 () =
        Utils.readResource' "Day9Input.txt"
        |> IntCode.parse
        |> IntCode.run [2L]
        |> Seq.exactlyOne

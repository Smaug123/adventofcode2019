namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day5 =

    let part1 () =
        let output =
            Utils.readResource' "Day5Input.txt"
            |> IntCode.parse
            |> IntCode.run [1L]

        // Check that all entries up til the last are zero, then output the last.
        use e = output.GetEnumerator ()
        let mutable prev = 0L
        while e.MoveNext () do
            if prev <> 0L then failwithf "Saw nonzero output %i" e.Current
            prev <- e.Current
        prev

    let part2 () =
        Utils.readResource' "Day5Input.txt"
        |> IntCode.parse
        |> IntCode.run [5L]
        |> Seq.exactlyOne

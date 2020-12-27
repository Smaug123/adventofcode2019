namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day5 =

    let part1 () =
        let program =
            Utils.readResource' "Day5Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split ","
            |> Array.map int
        let output = IntCode.run program [1]

        // Check that all entries up til the last are zero, then output the last.
        use e = output.GetEnumerator ()
        let mutable prev = 0
        while e.MoveNext () do
            if prev <> 0 then failwithf "Saw nonzero output %i" e.Current
            prev <- e.Current
        prev

    let part2 () =
        let program =
            Utils.readResource' "Day5Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split ","
            |> Array.map int
        let output = IntCode.run program [5]
        output
        |> Seq.exactlyOne

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
        let _, output = IntCode.run program [1]

        let rec verifyOutput (output : int list) =
            match output with
            | [] -> failwith "Empty output"
            | [ x ] -> x
            | x :: output ->
                if x <> 0 then failwithf "Saw nonzero output %i" x
                verifyOutput output
        verifyOutput output

    let part2 () =
        let program =
            Utils.readResource' "Day5Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split ","
            |> Array.map int
        let _, output = IntCode.run program [5]
        output
        |> List.exactlyOne

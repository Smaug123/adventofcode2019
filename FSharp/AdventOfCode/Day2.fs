namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day2 =

    let part1 () =
        let program =
            Utils.readResource' "Day2Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split ","
            |> Array.map int

        program.[1] <- 12
        program.[2] <- 2
        IntCode.execute program 0
        program.[0]

    let part2 () =
        let originalProgram =
            Utils.readResource' "Day2Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split ","
            |> Array.map int

        let scratchSpace = Array.zeroCreate originalProgram.Length
        let resetScratch () =
            System.Array.Copy (originalProgram, scratchSpace, originalProgram.Length)


        let rec goNoun (noun : int) =
            if noun >= 100 then None else

            let rec goVerb (verb : int) =
                if verb >= 100 then None else
                resetScratch ()
                scratchSpace.[1] <- noun
                scratchSpace.[2] <- verb
                IntCode.execute scratchSpace 0
                if scratchSpace.[0] = 19690720 then
                    Some (noun, verb)
                else goVerb (verb + 1)

            match goVerb 0 with
            | None -> goNoun (noun + 1)
            | Some (noun, verb) -> Some (noun, verb)

        match goNoun 0 with
        | None -> failwith "Couldn't find an answer"
        | Some (noun, verb) -> 100 * noun + verb


namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day2 =

    let part1 () =
        let program =
            Utils.readResource' "Day2Input.txt"
            |> IntCode.parse

        program.[1] <- 12L
        program.[2] <- 2L
        IntCode.run [] program |> Seq.iter ignore
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


        let rec goNoun (noun : int64) =
            if noun >= 100L then None else

            let rec goVerb (verb : int64) =
                if verb >= 100L then None else
                resetScratch ()
                scratchSpace.[1] <- noun
                scratchSpace.[2] <- verb
                IntCode.run [] scratchSpace |> Seq.iter ignore
                if scratchSpace.[0] = 19690720L then
                    Some (noun, verb)
                else goVerb (verb + 1L)

            match goVerb 0L with
            | None -> goNoun (noun + 1L)
            | Some (noun, verb) -> Some (noun, verb)

        match goNoun 0L with
        | None -> failwith "Couldn't find an answer"
        | Some (noun, verb) -> 100L * noun + verb


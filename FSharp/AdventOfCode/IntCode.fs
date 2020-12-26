namespace AdventOfCode

[<RequireQualifiedAccess>]
module IntCode =

    let rec execute (program : int array) (i : int) : unit =
        match program.[i] with
        | 99 -> ()
        | 1 ->
            program.[program.[i + 3]] <- program.[program.[i + 1]] + program.[program.[i + 2]]
            execute program (i + 4)
        | 2 ->
            program.[program.[i + 3]] <- program.[program.[i + 1]] * program.[program.[i + 2]]
            execute program (i + 4)
        | i -> failwithf "Unknown operation: %i" i

    let run (input : int seq) : int =
        let arr = input |> Array.ofSeq
        execute arr 0
        arr.[0]
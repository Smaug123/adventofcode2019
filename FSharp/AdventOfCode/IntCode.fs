namespace AdventOfCode

[<RequireQualifiedAccess>]
module IntCode =

    [<Measure>]
    type paramMode

    let values (program : int array) (parameterModes : int<paramMode>) (paramIndex : int) (expectedParamNumber : int) =
        let expectedParamNumber = expectedParamNumber + paramIndex

        let rec go (acc : int list) (parameterModes : int<paramMode>) (paramIndex : int) =
            if paramIndex >= expectedParamNumber then acc else
            let next =
                match parameterModes % 10<paramMode> with
                | 0<paramMode> ->
                    program.[program.[paramIndex]]
                | 1<paramMode> ->
                    program.[paramIndex]
                | _ -> failwithf "Unexpected parameter mode %i" parameterModes
            go (next :: acc) (parameterModes / 10) (paramIndex + 1)

        go [] parameterModes paramIndex
        |> List.rev

    let rec execute (program : int array) (input : int list) (i : int) (output : int list) : int list =
        let opCode, parameterModes =
            program.[i]
            |> fun x ->
                x % 100, (x / 100 * 1<paramMode>)

        match opCode with
        | 99 -> output |> List.rev
        | 1 ->
            let param1, param2 =
                values program parameterModes (i + 1) 2
                |> function | [x ; y] -> x, y | _ -> failwith "Oh no"
            program.[program.[i + 3]] <- param1 + param2
            execute program input (i + 4) output
        | 2 ->
            let param1, param2 =
                values program parameterModes (i + 1) 2
                |> function | [x ; y] -> x, y | _ -> failwith "Oh no"
            program.[program.[i + 3]] <- param1 * param2
            execute program input (i + 4) output
        | 3 ->
            match input with
            | [] -> failwith "Attempted to consume input for opcode 3"
            | x :: input ->
                program.[program.[i + 1]] <- x
                execute program input (i + 2) output
        | 4 ->
            let param = values program parameterModes (i + 1) 1 |> List.exactlyOne
            execute program input (i + 2) (param :: output)
        | 5 ->
            let param, next =
                values program parameterModes (i + 1) 2
                |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
            if param <> 0 then
                execute program input next output
            else
                execute program input (i + 3) output
        | 6 ->
            let param, next =
                values program parameterModes (i + 1) 2
                |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
            if param = 0 then
                execute program input next output
            else
                execute program input (i + 3) output
        | 7 ->
            let param1, param2 =
                values program parameterModes (i + 1) 2
                |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
            if param1 < param2 then
                program.[program.[i + 3]] <- 1
            else
                program.[program.[i + 3]] <- 0
            execute program input (i + 4) output
        | 8 ->
            let param1, param2 =
                values program parameterModes (i + 1) 2
                |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
            if param1 = param2 then
                program.[program.[i + 3]] <- 1
            else
                program.[program.[i + 3]] <- 0
            execute program input (i + 4) output
        | i -> failwithf "Unknown operation: %i" i

    let run (program : int array) (input : int list) : int * int list =
        let output = execute program input 0 []
        program.[0], output
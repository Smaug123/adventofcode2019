namespace AdventOfCode

open System.Collections.Generic

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

    let rec private execute (program : int array) (input : int IEnumerator) (i : int) : int seq =
        seq {
            let opCode, parameterModes =
                program.[i]
                |> fun x ->
                    x % 100, (x / 100 * 1<paramMode>)

            match opCode with
            | 99 -> ()
            | 1 ->
                let param1, param2 =
                    values program parameterModes (i + 1) 2
                    |> function | [x ; y] -> x, y | _ -> failwith "Oh no"
                program.[program.[i + 3]] <- param1 + param2
                yield! execute program input (i + 4)
            | 2 ->
                let param1, param2 =
                    values program parameterModes (i + 1) 2
                    |> function | [x ; y] -> x, y | _ -> failwith "Oh no"
                program.[program.[i + 3]] <- param1 * param2
                yield! execute program input (i + 4)
            | 3 ->
                match input.MoveNext () with
                | false -> failwith "Attempted to consume input for opcode 3, but there was none."
                | true ->
                    program.[program.[i + 1]] <- input.Current
                    yield! execute program input (i + 2)
            | 4 ->
                let param = values program parameterModes (i + 1) 1 |> List.exactlyOne
                yield param
                yield! execute program input (i + 2)
            | 5 ->
                let param, next =
                    values program parameterModes (i + 1) 2
                    |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
                if param <> 0 then
                    yield! execute program input next
                else
                    yield! execute program input (i + 3)
            | 6 ->
                let param, next =
                    values program parameterModes (i + 1) 2
                    |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
                if param = 0 then
                    yield! execute program input next
                else
                    yield! execute program input (i + 3)
            | 7 ->
                let param1, param2 =
                    values program parameterModes (i + 1) 2
                    |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
                if param1 < param2 then
                    program.[program.[i + 3]] <- 1
                else
                    program.[program.[i + 3]] <- 0
                yield! execute program input (i + 4)
            | 8 ->
                let param1, param2 =
                    values program parameterModes (i + 1) 2
                    |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
                if param1 = param2 then
                    program.[program.[i + 3]] <- 1
                else
                    program.[program.[i + 3]] <- 0
                yield! execute program input (i + 4)
            | i -> failwithf "Unknown operation: %i" i
        }

    let run (program : int array) (input : int seq) : int seq =
        seq {
            use enumerator = input.GetEnumerator ()
            yield! execute program enumerator 0
        }
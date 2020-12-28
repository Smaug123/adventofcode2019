namespace AdventOfCode

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module IntCode =

    let parse (s : string []) =
        s
        |> Array.exactlyOne
        |> fun i -> i.Split ","
        |> Array.map int64

    [<Measure>]
    type paramMode

    type Memory =
        {
            Initial : int64 array
            Sparse : Dictionary<int64, int64>
        }

    [<RequireQualifiedAccess>]
    module Memory =
        let get (index : int64) (memory : Memory) =
            if index < LanguagePrimitives.GenericZero then failwithf "Invalid memory access: %i" index
            if index < memory.Initial.GetLongLength 0 then memory.Initial.[int index] else
            match memory.Sparse.TryGetValue index with
            | false, _ ->
                memory.Sparse.Add (index, LanguagePrimitives.GenericZero)
                LanguagePrimitives.GenericZero
            | true, v -> v

        let set (index : int64) (value : int64) (memory : Memory) =
            if index < LanguagePrimitives.GenericZero then failwithf "Invalid memory access: set index %i to value %i" index value
            if index < memory.Initial.GetLongLength 0 then memory.Initial.[int index] <- value else
            memory.Sparse.[index] <- value

    let values (memory : Memory) (relativeBase : int64) (parameterModes : int<paramMode>) (paramIndex : int64) (expectedParamNumber : int) =
        let expectedParamNumber = int64 expectedParamNumber + paramIndex

        let rec go (acc : int64 list) (parameterModes : int<paramMode>) (paramIndex : int64) =
            if paramIndex >= expectedParamNumber then acc else
            let atIndex = Memory.get paramIndex memory
            let next =
                match parameterModes % 10<paramMode> with
                | 0<paramMode> ->
                    Memory.get atIndex memory
                | 1<paramMode> ->
                    atIndex
                | 2<paramMode> ->
                    Memory.get (atIndex + relativeBase) memory
                | _ -> failwithf "Unexpected parameter mode %i" parameterModes
            go (next :: acc) (parameterModes / 10) (paramIndex + 1L)

        go [] parameterModes paramIndex
        |> List.rev

    let setLocation (memory : Memory) (i : int64) (value : int64) (parameterMode : int<paramMode>) (relativeBase : int64) =
        match parameterMode with
        | 0<paramMode> ->
            Memory.set (Memory.get i memory) value memory
        | 1<paramMode> -> failwith "Unexpected param mode 1 for a memory set instruction"
        | 2<paramMode> ->
            Memory.set (Memory.get i memory + relativeBase) value memory
        | _ -> failwithf "Unrecognised param mode %i for a memory set instruction" parameterMode

    let rec private execute (memory : Memory) (input : int64 IEnumerator) (relativeBase : int64) (i : int64) : int64 seq =
        seq {
            let opCode, parameterModes =
                Memory.get i memory
                |> fun x ->
                    if x > int64 Int32.MaxValue || x < int64 Int32.MinValue then failwithf "Opcode %i didn't fit in an int" x
                    let x = int x
                    x % 100, (x / 100 * 1<paramMode>)

            match opCode with
            | 99 -> ()
            | 1 ->
                let param1, param2 =
                    values memory relativeBase parameterModes (i + 1L) 2
                    |> function | [x ; y] -> x, y | _ -> failwith "Oh no"
                setLocation memory (i + 3L) (param1 + param2) (parameterModes / 100) relativeBase
                yield! execute memory input relativeBase (i + 4L)
            | 2 ->
                let param1, param2 =
                    values memory relativeBase parameterModes (i + 1L) 2
                    |> function | [x ; y] -> x, y | _ -> failwith "Oh no"
                setLocation memory (i + 3L) (param1 * param2) (parameterModes / 100) relativeBase
                yield! execute memory input relativeBase (i + 4L)
            | 3 ->
                match input.MoveNext () with
                | false -> failwith "Attempted to consume input for opcode 3, but there was none."
                | true ->
                    setLocation memory (i + 1L) input.Current parameterModes relativeBase
                    yield! execute memory input relativeBase (i + 2L)
            | 4 ->
                let param = values memory relativeBase parameterModes (i + 1L) 1 |> List.exactlyOne
                yield param
                yield! execute memory input relativeBase (i + 2L)
            | 5 ->
                let param, next =
                    values memory relativeBase parameterModes (i + 1L) 2
                    |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
                if param <> 0L then
                    yield! execute memory input relativeBase next
                else
                    yield! execute memory input relativeBase (i + 3L)
            | 6 ->
                let param, next =
                    values memory relativeBase parameterModes (i + 1L) 2
                    |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
                if param = 0L then
                    yield! execute memory input relativeBase next
                else
                    yield! execute memory input relativeBase (i + 3L)
            | 7 ->
                let param1, param2 =
                    values memory relativeBase parameterModes (i + 1L) 2
                    |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
                if param1 < param2 then
                    setLocation memory (i + 3L) 1L (parameterModes / 100) relativeBase
                else
                    setLocation memory (i + 3L) 0L (parameterModes / 100) relativeBase
                yield! execute memory input relativeBase (i + 4L)
            | 8 ->
                let param1, param2 =
                    values memory relativeBase parameterModes (i + 1L) 2
                    |> function | [ x ; y ] -> x, y | _ -> failwith "unexpected parameter count"
                if param1 = param2 then
                    setLocation memory (i + 3L) 1L (parameterModes / 100) relativeBase
                else
                    setLocation memory (i + 3L) 0L (parameterModes / 100) relativeBase
                yield! execute memory input relativeBase (i + 4L)
            | 9 ->
                let param = values memory relativeBase parameterModes (i + 1L) 1 |> List.exactlyOne
                yield! execute memory input (relativeBase + param) (i + 2L)
            | i -> failwithf "Unknown operation: %i" i
        }

    let run (input : int64 seq) (program : int64 array) : int64 seq =
        seq {
            use enumerator = input.GetEnumerator ()
            yield! execute { Initial = program ; Sparse = Dictionary () } enumerator 0L 0L
        }

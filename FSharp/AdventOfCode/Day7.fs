namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day7 =

    let rec insertOnce (x : 'a) (xs : 'a list) =
        match xs with
        | [] -> [[x]]
        | y :: xs ->
            (x :: y :: xs)
            ::
            (insertOnce x xs
            |> List.map (fun i -> y :: i))

    let rec permutations (xs : 'a list) =
        match xs with
        | [] -> [[]]
        | x :: xs ->
            permutations xs
            |> List.collect (insertOnce x)

    let part1 () =
        let program =
            Utils.readResource' "Day7Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split ","
            |> Array.map int

        let scratch = Array.zeroCreate program.Length
        let resetScratch () = System.Array.Copy (program, scratch, program.Length)

        permutations [0..4]
        |> List.map (fun perm ->
            perm
            |> List.fold (fun inputSignal phaseSetting ->
                resetScratch ()
                IntCode.run scratch [phaseSetting ; inputSignal] |> Seq.exactlyOne
            ) 0
        )
        |> List.max

    let part2 () =
        let program =
            Utils.readResource' "Day7Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split ","
            |> Array.map int

        let scratches = [| for _ in 1..5 do yield Array.zeroCreate program.Length |]
        let resetScratches () =
            for i in 0..4 do
                System.Array.Copy (program, scratches.[i], program.Length)

        permutations [5..9]
        |> List.map (fun l ->
            resetScratches ()

            let r, s, t, u, v =
                match l with
                | [r ; s ; t ; u ; v] -> r, s, t, u, v
                | _ -> failwith "logic error"

            let mutable value = 0
            let mutable go = true
            let loopy =
                seq {
                    yield r
                    while go do
                        yield value
                }
            let a = IntCode.run scratches.[0] loopy
            let b = IntCode.run scratches.[0] (seq { yield s ; yield! a })
            let c = IntCode.run scratches.[0] (seq { yield t ; yield! b })
            let d = IntCode.run scratches.[0] (seq { yield u ; yield! c })
            let e =
                seq {
                    for i in IntCode.run scratches.[0] (seq { yield v ; yield! d }) do
                        value <- i
                        yield i
                    go <- false
                }

            e |> Seq.last
        )
        |> List.max

        (*
        permutations [5..9]
        |> List.map (fun perm ->
            resetScratches ()
            // Problem: we need to provide information to a running program.
            let rec go (startVal : int) =
                perm
                |> List.fold (fun (i, inputSignal) phaseSetting ->
                    i + 1, IntCode.run scratches.[i] [phaseSetting ; inputSignal] |> Seq.exactlyOne
                ) (0, startVal)
            go 0
        )
        |> List.max
        *)
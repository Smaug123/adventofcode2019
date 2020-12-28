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
            |> IntCode.parse

        let scratch = Array.zeroCreate program.Length
        let resetScratch () = System.Array.Copy (program, scratch, program.Length)

        permutations [0L..4L]
        |> List.map (fun perm ->
            perm
            |> List.fold (fun inputSignal phaseSetting ->
                resetScratch ()
                IntCode.run [phaseSetting ; inputSignal] scratch |> Seq.exactlyOne
            ) 0L
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

        permutations [5L..9L]
        |> List.map (fun l ->
            resetScratches ()

            let r, s, t, u, v =
                match l with
                | [r ; s ; t ; u ; v] -> r, s, t, u, v
                | _ -> failwith "logic error"

            let mutable value = 0L
            let mutable go = true
            let loopy =
                seq {
                    yield r
                    while go do
                        yield value
                }
            let a = IntCode.run loopy scratches.[0]
            let b = IntCode.run (seq { yield s ; yield! a }) scratches.[0]
            let c = IntCode.run (seq { yield t ; yield! b }) scratches.[0]
            let d = IntCode.run (seq { yield u ; yield! c }) scratches.[0]
            let e =
                seq {
                    for i in IntCode.run (seq { yield v ; yield! d }) scratches.[0] do
                        value <- i
                        yield i
                    go <- false
                }

            e |> Seq.last
        )
        |> List.max

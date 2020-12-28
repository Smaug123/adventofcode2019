namespace AdventOfCode

open System
open System.Text
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day8 =

    let part1 () =
        let input =
            Utils.readResource' "Day8Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.ToCharArray ()
            |> fun i -> Array.splitInto (i.Length / (6 * 25)) i

        let rec go (numZeros : int) (numOnes : int) (numTwos : int) (i : int) (input : char[]) =
            if i >= input.Length then (numZeros, numOnes, numTwos) else
            match input.[i] with
            | '0' -> go (numZeros + 1) numOnes numTwos (i + 1) input
            | '1' -> go numZeros (numOnes + 1) numTwos (i + 1) input
            | '2' -> go numZeros numOnes (numTwos + 1) (i + 1) input
            | _ -> failwith "unexpected char"

        input
        |> Array.map (go 0 0 0 0)
        |> Array.minBy (fun (a, _, _) -> a)
        |> fun (_, b, c) -> b * c

    let part2 () =
        let rec go (i : int) (arr : char []) =
            if i >= arr.Length then 2 else
            match arr.[i] with
            | '0' -> 0
            | '1' -> 1
            | '2' -> go (i + 1) arr
            | _ -> failwith "I was lazy and didn't model the domain"

        let width = 6
        let height = 25
        let image =
            Utils.readResource' "Day8Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.ToCharArray ()
            |> fun i -> Array.splitInto (i.Length / (width * height)) i
            |> Array.transpose
            |> Array.map (go 0)
            |> Array.splitInto width

        image
        |> Array.map (fun arr -> arr |> Array.map (fun i -> if i = 1 then 'X' else ' ') |> String)
        |> Array.toList

namespace AdventOfCode

open System.Collections.Generic
open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day3 =

    type Direction =
        | Up
        | Down
        | Left
        | Right

    [<RequireQualifiedAccess>]
    module Direction =
        let parse (c : char) =
            match c with
            | 'U' -> Direction.Up
            | 'D' -> Direction.Down
            | 'L' -> Direction.Left
            | 'R' -> Direction.Right
            | _ -> failwithf "Unexpected char: %c" c

        let rec travel (dir : Direction) (steps : int) (acc : Dictionary<int * int, int>) ((x, y) as start : int * int) (distance : int) =
            acc.TryAdd (start, steps) |> ignore
            if distance = 0 then start, steps else
            let newStart =
                match dir with
                | Up -> (x, y + 1)
                | Down -> (x, y - 1)
                | Left -> (x - 1, y)
                | Right -> (x + 1, y)
            travel dir (steps + 1) acc newStart (distance - 1)

    let rec getPoints (soFar : Dictionary<int * int, int>) (steps : int) (position : int * int) (path : (Direction * int) list) =
        match path with
        | [] -> soFar
        | (direction, distance) :: path ->
            let newPoint, steps = Direction.travel direction steps soFar position distance
            getPoints soFar steps newPoint path

    let part1 () =
        let path1, path2 =
            Utils.readResource' "Day3Input.txt"
            |> Array.map (fun i -> i.Split ',' |> Array.map (fun i -> Direction.parse i.[0], (int i.[1..])) |> Array.toList)
            |> function
                | [| x ; y |] -> x, y
                | arr -> failwithf "Unexpected input length %i" arr.Length

        let points1 = getPoints (Dictionary()) 0 (0, 0) path1
        let points2 = getPoints (Dictionary()) 0 (0, 0) path2

        let points = HashSet(points1.Keys)
        points.IntersectWith(points2.Keys)
        points.Remove (0, 0) |> ignore

        points
        |> Seq.map (fun (i, j) -> abs i + abs j)
        |> Seq.min

    let part2 () =
        let path1, path2 =
            Utils.readResource' "Day3Input.txt"
            |> Array.map (fun i -> i.Split ',' |> Array.map (fun i -> Direction.parse i.[0], (int i.[1..])) |> Array.toList)
            |> function
                | [| x ; y |] -> x, y
                | arr -> failwithf "Unexpected input length %i" arr.Length

        let points1 = getPoints (Dictionary()) 0 (0, 0) path1
        let points2 = getPoints (Dictionary()) 0 (0, 0) path2

        let points = HashSet(points1.Keys)
        points.IntersectWith(points2.Keys)
        points.Remove (0, 0) |> ignore

        points
        |> Seq.map (fun (i, j) -> points1.[i, j] + points2.[i, j])
        |> Seq.min

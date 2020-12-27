namespace AdventOfCode

open AdventOfCode.Internals
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Day6 =

    type ContainsSanta = ContainsSanta of bool
    type ContainsYou = ContainsYou of bool

    [<NoComparison ; CustomEquality>]
    type Tree =
        | Leaf of string
        | Branch of Map<string, Tree> * string * ContainsSanta * ContainsYou
        override this.Equals (other : obj) =
            match other with
            | :? Tree as other ->
                match this, other with
                | Leaf a, Leaf b -> a = b
                | Branch _, Leaf _
                | Leaf _, Branch _ -> false
                | Branch (_, name1, _, _), Branch (_, name2, _, _) -> name1 = name2
            | _ -> false
        override this.GetHashCode () =
            match this with
            | Leaf a -> a.GetHashCode ()
            | Branch (_, a, _, _) -> a.GetHashCode ()

    [<RequireQualifiedAccess>]
    module Tree =
        type private Partial = Partial of string list

        /// The first entry is the thing being orbited; the second entry is the thing doing the orbited.
        /// The thing being orbited (the first entry) is the parent of the thing doing the orbiting.
        /// Return also the path to YOU and SAN.
        let make (rules : (string * string) list) : Tree =
            let state = Dictionary<string, string list> ()
            let get (name : string) =
                match state.TryGetValue name with
                | false, _ ->
                    let r = []
                    state.[name] <- r
                    r
                | true, v -> v

            let rec go (rules : (string * string) list) : unit =
                match rules with
                | [] -> ()
                | (parent, child) :: rules ->
                    let p = get parent
                    state.[parent] <- child :: p
                    go rules

            go rules

            let built = Dictionary<string, Tree> ()
            let rec go (stack : string list) =
                match stack with
                | [] -> ()
                | s :: stack ->
                    if built.ContainsKey s then go stack else
                    match state.TryGetValue s with
                    | false, _
                    | true, [] ->
                        built.[s] <- Leaf s
                        go stack
                    | true, xs ->
                        let toMake, made =
                            xs
                            |> List.map (fun x -> match built.TryGetValue x with | false, _ -> x, None | true, v -> x, Some v)
                            |> List.partition (fun (_, child) -> Option.isNone child)
                        match toMake with
                        | [] ->
                            let children = made |> List.map (fun (x, v) -> x, Option.get v)
                            let anyContainsSanta =
                                children
                                |> List.exists (fun (_, t) -> match t with | Leaf "SAN" | Branch (_, _, ContainsSanta true, _) -> true | _ -> false)
                            let anyContainsYou =
                                children
                                |> List.exists (fun (_, t) -> match t with | Leaf "YOU" | Branch (_, _, _, ContainsYou true) -> true | _ -> false)
                            built.[s] <- Branch (Map.ofList children, s, ContainsSanta anyContainsSanta, ContainsYou anyContainsYou)
                            go stack
                        | toMake ->
                            let toMake = toMake |> List.map fst
                            go (toMake @ s :: stack)

            go ["COM"]

            built.["COM"]

        /// Visit every node, accumulating some state.
        let rec numberOfChildren (depth : int) (t : Tree) : int =
            match t with
            | Leaf _ -> depth
            | Branch (children, _, _, _) ->
                children
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.map (fun tree ->
                    numberOfChildren (depth + 1) tree
                )
                |> Seq.sum
                |> (+) depth

        let rec pathToSanta (acc : string list) (t : Tree) =
            match t with
            | Leaf _ -> acc
            | Branch (children, name, ContainsSanta true, _) ->
                let _, child =
                    children
                    |> Map.toSeq
                    |> Seq.find (fun (_, tree) -> match tree with | Leaf "SAN" | Branch (_, _, ContainsSanta true, _) -> true | _ -> false)
                pathToSanta (name :: acc) child
            | _ -> failwith "Stopped knowing where Santa is!"

        let rec pathToYou (acc : string list) (t : Tree) =
            match t with
            | Leaf _ -> acc
            | Branch (children, name, _, ContainsYou true) ->
                let _, child =
                    children
                    |> Map.toSeq
                    |> Seq.find (fun (_, tree) -> match tree with | Leaf "YOU" | Branch (_, _, _, ContainsYou true) -> true | _ -> false)
                pathToYou (name :: acc) child
            | _ -> failwith "Stopped knowing where you are!"

    let part1 () =
        Utils.readResource "Day6Input.txt"
        |> List.map (fun i -> i.Split ')' |> function | [| a ; b |] -> a, b | xs -> failwithf "Unexpected format: %+A" xs)
        |> Tree.make
        |> Tree.numberOfChildren 0

    let part2 () =
        let tree =
            Utils.readResource "Day6Input.txt"
            |> List.map (fun i -> i.Split ')' |> function | [| a ; b |] -> a, b | xs -> failwithf "Unexpected format: %+A" xs)
            |> Tree.make
        let p1 = Tree.pathToSanta [] tree
        let p2 = Tree.pathToYou [] tree
        let s1 = Set.ofList p1
        let rec go (i : int) (p2 : string list) =
            match p2 with
            | [] -> failwith "Somehow hit the end"
            | nextToSanta :: p2 ->
                if s1.Contains nextToSanta then i, nextToSanta else
                go (i + 1) p2
        let fromSanta, nextToSanta = go 0 p2
        let toYou = p1 |> Seq.takeWhile ((<>) nextToSanta) |> Seq.length

        fromSanta + toYou

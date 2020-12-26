namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day1 =

    let inline getRequirement (mass : int) = (mass / 3) - 2

    let part1 () =
        Utils.readResource "Day1Input.txt"
        |> List.map int
        |> List.map getRequirement
        |> List.sum

    let rec totalFuel (acc : int) (weight : int) =
        let newFuel = getRequirement weight
        if newFuel <= 0 then acc else totalFuel (acc + newFuel) newFuel

    let part2 () =


        Utils.readResource "Day1Input.txt"
        |> List.map int
        |> List.map (totalFuel 0)
        |> List.sum
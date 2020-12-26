namespace AdventOfCode

open AdventOfCode.Internals

[<RequireQualifiedAccess>]
module Day4 =

    let rec containsAdjacentDigits (i : int) =
        if i < 10 then false else
        let shift = i / 10
        (i % 10 = shift % 10) || containsAdjacentDigits shift

    let rec increasing (i : int) =
        if i < 10 then true else
        let shift = i / 10
        (i % 10 >= shift % 10) && increasing shift

    let part1Match (i : int) =
        containsAdjacentDigits i && increasing i

    let containsExactlyAdjacentDigits (i : int) =
        let rec go i shift iMod shiftMod =
            if i < 10 then false
            elif i < 100 then i % 11 = 0
            else
            let doubleShift = shift / 10
            let doubleShiftMod = doubleShift % 10

            if iMod = shiftMod then
                if iMod <> doubleShiftMod then true else
                // ignore this group
                let rec getNext (i : int) =
                    if i < 10 then None else
                    if i % 10 = iMod then getNext (i / 10)
                    else
                        let shift = i / 10
                        Some (i, shift, i % 10, shift % 10)
                match getNext doubleShift with
                | None -> false
                | Some (i, shift, iMod, shiftMod) -> go i shift iMod shiftMod
            else
                go shift doubleShift shiftMod doubleShiftMod

        if i < 10 then false
        elif i < 100 then i % 11 = 0
        else
            let shift = i / 10
            go i shift (i % 10) (shift % 10)

    let part2Match (i : int) =
        increasing i && containsExactlyAdjacentDigits i

    let part1 () =
        let min, max =
            Utils.readResource' "Day4Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split '-'
            |> Array.map int
            |> function | [| x ; y |] -> x, y | a -> failwithf "Unexpected format: %+A" a

        let rec go (acc : int) (i : int) =
            if i > max then acc else
            if part1Match i then go (acc + 1) (i + 1) else go acc (i + 1)

        go 0 min

    let part2 () =
        let min, max =
            Utils.readResource' "Day4Input.txt"
            |> Array.exactlyOne
            |> fun i -> i.Split '-'
            |> Array.map int
            |> function | [| x ; y |] -> x, y | a -> failwithf "Unexpected format: %+A" a

        let rec go (acc : int) (i : int) =
            if i > max then acc else
            if part2Match i then go (acc + 1) (i + 1) else go acc (i + 1)

        go 0 min

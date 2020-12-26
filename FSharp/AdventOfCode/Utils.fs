namespace AdventOfCode.Internals

open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Utils =
    type private Dummy = class end

    let readResource' (name : string) : string array =
        let asm = Assembly.GetAssembly typeof<Dummy>
        use stream = asm.GetManifestResourceStream (sprintf "AdventOfCode.%s" name)
        let s =
            use reader = new StreamReader(stream)
            reader.ReadToEnd()
        s.Split('\r', '\n')

    let inline readResource (name : string) : string list =
        readResource' name |> List.ofArray

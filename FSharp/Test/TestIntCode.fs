namespace AdventOfCode.Test

open AdventOfCode
open NUnit.Framework

[<TestFixture>]
module TestIntCode =

    let private test (program : int64 seq) (input : int64 list) (expectedMemory : #seq<int64> option) (expectedOutput : int64 list) =
        let arr = program |> Seq.toArray
        let output = IntCode.run input arr
        // Order is important here: `output` is lazy.
        CollectionAssert.AreEqual (expectedOutput, output)
        match expectedMemory with
        | Some expectedMemory ->
            CollectionAssert.AreEqual (expectedMemory, arr)
        | None -> ()

    [<Test>]
    let ``Examples from day 2`` () =
        test ("1,9,10,3,2,3,11,0,99,30,40,50".Split "," |> Array.map int64) [] (Some [ 3500L; 9L; 10L; 70L; 2L; 3L; 11L; 0L; 99L; 30L; 40L; 50L ]) []
        test ("1,0,0,0,99".Split "," |> Array.map int64) [] (Some [ 2L;0L;0L;0L;99L ]) []
        test ("2,3,0,3,99".Split "," |> Array.map int64) [] (Some [ 2L;3L;0L;6L;99L ]) []
        test ("2,4,4,5,99,0".Split "," |> Array.map int64) [] (Some [ 2L;4L;4L;5L;99L;9801L ]) []
        test ("1,1,1,4,99,5,6,0,99".Split "," |> Array.map int64) [] (Some [ 30L;1L;1L;4L;2L;5L;6L;0L;99L ]) []

    [<Test>]
    let ``Tests from Day 5`` () =
        for i in [0L;1L;7L;9L;10L;1000L;-100L] do
            test [3L;9L;8L;9L;10L;9L;4L;9L;99L;-1L;8L] [i] (Some [3L;9L;8L;9L;10L;9L;4L;9L;99L;0L;8L]) [0L]
        test [3L;9L;8L;9L;10L;9L;4L;9L;99L;-1L;8L] [8L] (Some [3L;9L;8L;9L;10L;9L;4L;9L;99L;1L;8L]) [1L]

        for i in [0L;1L;2L;6L;7L;-100L] do
            test [3L;9L;7L;9L;10L;9L;4L;9L;99L;-1L;8L] [i] (Some [3L;9L;7L;9L;10L;9L;4L;9L;99L;1L;8L]) [1L]
        for i in [8L;9L;10L;1000L] do
            test [3L;9L;7L;9L;10L;9L;4L;9L;99L;-1L;8L] [i] (Some [3L;9L;7L;9L;10L;9L;4L;9L;99L;0L;8L]) [0L]

        for i in [0L;1L;2L;7L;9L;10L;1000L;-100L] do
            test [3L;3L;1108L;-1L;8L;3L;4L;3L;99L] [i] (Some [3L;3L;1108L;0L;8L;3L;4L;3L;99L]) [0L]
        test [3L;3L;1108L;-1L;8L;3L;4L;3L;99L] [8L] (Some [3L;3L;1108L;1L;8L;3L;4L;3L;99L]) [1L]

        for i in [0L;1L;2L;6L;7L;-100L] do
            test [3L;3L;1107L;-1L;8L;3L;4L;3L;99L] [i] (Some [3L;3L;1107L;1L;8L;3L;4L;3L;99L]) [1L]
        for i in [8L;9L;10L;1000L] do
            test [3L;3L;1107L;-1L;8L;3L;4L;3L;99L] [i] (Some [3L;3L;1107L;0L;8L;3L;4L;3L;99L]) [0L]

        for i in [-1L;-2L;1L;2L;6L;7L;-100L] do
            test [3L;12L;6L;12L;15L;1L;13L;14L;13L;4L;13L;99L;-1L;0L;1L;9L] [i] None [1L]
        test [3L;12L;6L;12L;15L;1L;13L;14L;13L;4L;13L;99L;-1L;0L;1L;9L] [0L] None [0L]

        for i in [-1L;-2L;1L;2L;6L;7L;-100L] do
            test [3L;3L;1105L;-1L;9L;1101L;0L;0L;12L;4L;12L;99L;1L] [i] None [1L]
        test [3L;3L;1105L;-1L;9L;1101L;0L;0L;12L;4L;12L;99L;1L] [0L] None [0L]

        let arr =
            [
                3L;21L;1008L;21L;8L;20L;1005L;20L;22L;107L;8L;21L;20L;1006L;20L;31L;
                1106L;0L;36L;98L;0L;0L;1002L;21L;125L;20L;4L;20L;1105L;1L;46L;104L;
                999L;1105L;1L;46L;1101L;1000L;1L;20L;4L;20L;1105L;1L;46L;98L;99L
            ]
        for i in [-1L;-2L;1L;2L;6L;7L;-100L] do
            test arr [i] None [999L]
        test arr [8L] None [1000L]
        for i in [9 ; 10 ; 10000] do
            test arr [int64 i] None [1001L]

    [<Test>]
    let ``Examples from day 9`` () =
        let quine = [ 109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L ]
        test quine [] None quine

        test [|104L;1125899906842624L;99L|] [] None [1125899906842624L]
        test [|1102L;34915192L;34915192L;7L;4L;7L;99L;0L|] [] None [1219070632396864L]

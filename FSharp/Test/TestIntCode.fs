namespace AdventOfCode.Test

open AdventOfCode
open NUnit.Framework

[<TestFixture>]
module TestIntCode =

    let private test (program : int seq) (input : int list) (expectedMemory : #seq<int> option) (expectedOutput : int list) =
        let arr = program |> Seq.toArray
        let _, output = IntCode.run arr input
        match expectedMemory with
        | Some expectedMemory ->
            CollectionAssert.AreEqual (expectedMemory, arr)
        | None -> ()
        CollectionAssert.AreEqual (expectedOutput, output)

    [<Test>]
    let ``Examples from day 2`` () =
        test ("1,9,10,3,2,3,11,0,99,30,40,50".Split "," |> Array.map int) [] (Some [ 3500 ; 9 ; 10 ; 70 ; 2 ; 3 ; 11 ; 0 ; 99 ; 30 ; 40 ; 50 ]) []
        test ("1,0,0,0,99".Split "," |> Array.map int) [] (Some [ 2;0;0;0;99 ]) []
        test ("2,3,0,3,99".Split "," |> Array.map int) [] (Some [ 2;3;0;6;99 ]) []
        test ("2,4,4,5,99,0".Split "," |> Array.map int) [] (Some [ 2;4;4;5;99;9801 ]) []
        test ("1,1,1,4,99,5,6,0,99".Split "," |> Array.map int) [] (Some [ 30;1;1;4;2;5;6;0;99 ]) []

    [<Test>]
    let ``Tests from Day 5`` () =
        for i in [0;1;7;9;10;1000;-100] do
            test [3;9;8;9;10;9;4;9;99;-1;8] [i] (Some [3;9;8;9;10;9;4;9;99;0;8]) [0]
        test [3;9;8;9;10;9;4;9;99;-1;8] [8] (Some [3;9;8;9;10;9;4;9;99;1;8]) [1]

        for i in [0;1;2;6;7;-100] do
            test [3;9;7;9;10;9;4;9;99;-1;8] [i] (Some [3;9;7;9;10;9;4;9;99;1;8]) [1]
        for i in [8;9;10;1000] do
            test [3;9;7;9;10;9;4;9;99;-1;8] [i] (Some [3;9;7;9;10;9;4;9;99;0;8]) [0]

        for i in [0;1;2;7;9;10;1000;-100] do
            test [3;3;1108;-1;8;3;4;3;99] [i] (Some [3;3;1108;0;8;3;4;3;99]) [0]
        test [3;3;1108;-1;8;3;4;3;99] [8] (Some [3;3;1108;1;8;3;4;3;99]) [1]

        for i in [0;1;2;6;7;-100] do
            test [3;3;1107;-1;8;3;4;3;99] [i] (Some [3;3;1107;1;8;3;4;3;99]) [1]
        for i in [8;9;10;1000] do
            test [3;3;1107;-1;8;3;4;3;99] [i] (Some [3;3;1107;0;8;3;4;3;99]) [0]

        for i in [-1;-2;1;2;6;7;-100] do
            test [3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9] [i] None [1]
        test [3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9] [0] None [0]

        for i in [-1;-2;1;2;6;7;-100] do
            test [3;3;1105;-1;9;1101;0;0;12;4;12;99;1] [i] None [1]
        test [3;3;1105;-1;9;1101;0;0;12;4;12;99;1] [0] None [0]

        let arr =
            [
                3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;
                1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;
                999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99
            ]
        for i in [-1;-2;1;2;6;7;-100] do
            test arr [i] None [999]
        test arr [8] None [1000]
        for i in [9 ; 10 ; 10000] do
            test arr [i] None [1001]

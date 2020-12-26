namespace AdventOfCode.Test

open AdventOfCode
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestIntCode =

    let private test (input : int seq) (expected : int seq) =
        let arr = input |> Seq.toArray
        IntCode.execute arr 0
        CollectionAssert.AreEqual (expected, arr)

    [<Test>]
    let ``Examples from day 2`` () =
        test ("1,9,10,3,2,3,11,0,99,30,40,50".Split "," |> Array.map int) [ 3500 ; 9 ; 10 ; 70 ; 2 ; 3 ; 11 ; 0 ; 99 ; 30 ; 40 ; 50 ]
        test ("1,0,0,0,99".Split "," |> Array.map int) [ 2;0;0;0;99 ]
        test ("2,3,0,3,99".Split "," |> Array.map int) [ 2;3;0;6;99 ]
        test ("2,4,4,5,99,0".Split "," |> Array.map int) [ 2;4;4;5;99;9801 ]
        test ("1,1,1,4,99,5,6,0,99".Split "," |> Array.map int) [ 30;1;1;4;2;5;6;0;99 ]

module AoC2019.Day2.Tests

open NUnit.Framework
open FsUnit
open System.IO


let input =     
    File.ReadAllText("./Week2InputA.txt").Split(',') 
        |> Array.map System.Int32.Parse

[<Test>]
let ``Day2 Example`` () = 
    run [|1;0;0;0;99|] 0 |> should equivalent [|2;0;0;0;99|]
    run [|2;3;0;3;99|] 0 |> should equivalent [|2;3;0;6;99|]
    run [|2;4;4;5;99;0|] 0 |> should equivalent [|2;4;4;5;99;9801|]
    run [|1;1;1;4;99;5;6;0;99|] 0 |> should equivalent [|30;1;1;4;2;5;6;0;99|]
    run [|1;9;10;3;2;3;11;0;99;30;40;50|] 0 |> should equivalent [|3500;9;10;70;2;3;11;0;99;30;40;50|]

[<Test>]
let ``Day2 A`` () =
    Array.set input 1 12
    Array.set input 2 2
    let result = run input 0
    Array.get result 0 |> should equal 4138658

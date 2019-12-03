module AoC2019.Day2.Tests

open NUnit.Framework
open FsUnit
open System.IO

let input =     
    File.ReadAllText("./Week2InputA.txt").Split(',') 
        |> Array.map System.Int32.Parse

let test input = 
    run (Intcode input) 0 |> fun (Intcode x) -> x 

[<Test>]
let ``Day2 Example`` () =     
    test [|1;0;0;0;99|] |> should equivalent [|2;0;0;0;99|]
    test [|2;3;0;3;99|] |> should equivalent [|2;3;0;6;99|]
    test [|2;4;4;5;99;0|] |> should equivalent [|2;4;4;5;99;9801|]
    test [|1;1;1;4;99;5;6;0;99|] |> should equivalent [|30;1;1;4;2;5;6;0;99|]
    test [|1;9;10;3;2;3;11;0;99;30;40;50|] |> should equivalent [|3500;9;10;70;2;3;11;0;99;30;40;50|]

[<Test>]
let ``Day2 A`` () =    
    runValue (Intcode input) 12 2 |> should equal 4138658

[<Test>]
let ``Day2 B, by brute force...`` () =    
    match (bruteForce (Intcode input) 19690720) with
    | Some (n,v) -> 
        let result = (100 * n) + v
        result |> should equal 7264
    | None -> failwith "nope"
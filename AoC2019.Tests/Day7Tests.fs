module AoC2019.Day7.Tests

open NUnit.Framework
open FsUnit
open System.IO

open AoC2019.Intcode

[<Test>]
let ``Day 7 Examples`` () =
    let program = Program [3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0]
    maxThruster program |> should equal 43210

[<Test>]
let ``Day 7 AllSpots`` () = 
    inAllSpots 1 [2;3] |> should equivalent [[1;2;3];[2;1;3];[2;3;1]]
    inAllSpots 1 [2;3;4] |> should equivalent [[1;2;3;4;];[2;1;3;4];[2;3;1;4];[2;3;4;1]]

[<Test>]
let ``Day 7 Permutations`` () = 
    permutations [1;2] |> should equivalent [[1;2];[2;1]]

let program =     
    File.ReadAllText("./Dag7Input.txt").Split(',') 
        |> List.ofArray
        |> List.map System.Int32.Parse
        |> Program

[<Test>]
let ``Day 7A`` () =
    maxThruster program |> should equal 65464

[<Test>]
let ``Day 7B Example`` () = 
    let program = Program [3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5]
    maxThrusterLoop program |> should equal 139629729

[<Test>]
let ``Day 7B`` () =
    maxThrusterLoop program |> should equal 0
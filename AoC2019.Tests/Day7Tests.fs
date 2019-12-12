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
    maxThruster program |> should equal 0
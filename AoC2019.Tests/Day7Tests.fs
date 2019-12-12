module AoC2019.Day7.Tests

open NUnit.Framework
open FsUnit
open System.IO

open AoC2019.Intcode

[<Test>]
let ``Day 7 Examples`` () =
    let program = Program [3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0]
    maxThruster program |> should equal 43210
module AoC2019.Day8.Tests

open NUnit.Framework
open FsUnit
open System.IO

open AoC2019.Shared

[<Test>]
let ``Day 8 Examples`` () =
    checksum 3 2 (toList 123456789012L) |> should equal 1

let input = File.ReadAllText("./Dag8Input.txt").ToCharArray()
                |> Array.map string
                |> Array.map System.Int32.Parse
                |> List.ofArray

[<Test>]
let ``Day 8 A`` () =                
    checksum 25 6 input |> should equal 1620


[<Test>]
let ``Day 8 B`` () =                
    splitInLayers 25 6 input |> flatten |> (print 25)
    //BCYEF
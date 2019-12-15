module AoC2019.Day9.Tests

open NUnit.Framework
open FsUnit
open System.IO

open AoC2019.Shared
open AoC2019.Intcode

let noinput () = failwith "nope"

let program =     
    File.ReadAllText("./Dag9Input.txt").Split(',') 
        |> List.ofArray
        |> List.map System.Int32.Parse
        |> Program

[<Test>]
let ``Day 9 A Quine`` () =
    let raw = [109;1;204;-1;1001;100;1;100;1008;100;16;101;1006;101;0;99]
    let quine = Program raw
    let outputList = System.Collections.Generic.List<int>()
    let output x = 
        outputList.Add x
    
    run (quine,noinput,output) |> ignore
    raw |> should equivalent (List.ofSeq outputList)

[<Test>]
let ``Day 9 A Like Big Numbers and cannot lie`` () =
    let program = Program [1102;34915192;34915192;7;4;7;99;0]
    let mutable output = -1
    run (program, noInput, fun x -> output <- x) |> ignore
    output |> should equal -1

//[<Test>]
//let ``Day 9 A Like Big Numbers and cannot lie 2`` () =
//    let program = Program [104L;1125899906842624L;99L]
//    let mutable output = -1
//    run (program, noInput, fun x -> output <- x) |> ignore
//    output |> should equal 1125899906842624L
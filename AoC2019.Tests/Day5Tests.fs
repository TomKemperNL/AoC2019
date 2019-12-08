module AoC2019.Day5.Tests

open NUnit.Framework
open FsUnit
open System.IO

open AoC2019.Intcode


[<Test>]
let ``Day5 echo IO`` () =     
    let input () = 5
    let mutable out = -1
    let output x = 
        out <- x
    run (Program [3;0;4;0;99]) 0 input output |> ignore
    out |> should equal 5

[<Test>]
let ``Day 5 parameter modes`` () =
    parseInstruction 1002 |> should equal (Multiply, [Position; Immediate; Position])

let program =     
    File.ReadAllText("./Dag5Input.txt").Split(',') 
        |> List.ofArray
        |> List.map System.Int32.Parse

[<Test>]
let ``Day5 A`` () =
    let input () = 1
    let out = new System.Collections.Generic.List<int>()
    let output x = 
        out.Add(x)
    run (Program program) 0 input output |> ignore
    let (diag, tests) = match Seq.rev out |> List.ofSeq with
    | diag::tests -> (diag, tests)
    | _ -> failwith "Nope"
    
    Seq.sum tests |> should equal 0
    diag |> should equal 5074395

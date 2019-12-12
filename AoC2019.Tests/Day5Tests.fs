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
    run ((Program [3;0;4;0;99]),input, output) |> ignore
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
    run ((Program program), input, output) |> ignore
    let (diag, tests) = match Seq.rev out |> List.ofSeq with
    | diag::tests -> (diag, tests)
    | _ -> failwith "Nope"
    
    Seq.sum tests |> should equal 0
    diag |> should equal 5074395
    
[<Test>]
let ``Day5 B Examples`` () =
    let program = [3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9]
    let mutable out = -1
    let output x = 
        out <- x
    run ((Program program), (fun () -> 0), output) |> ignore
    out |> should equal 0


[<Test>]
let ``Day5 B`` () =
    let input () = 5
    let out = new System.Collections.Generic.List<int>()
    let output x = 
        out.Add(x)
    run ((Program program), input, output) |> ignore
    let diag = match Seq.rev out |> List.ofSeq with
    | [diag] -> diag
    | _ -> failwith "Nope"    
    
    diag |> should equal 8346937
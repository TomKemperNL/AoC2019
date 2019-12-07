module AoC2019.Day5.Tests

open NUnit.Framework
open FsUnit

open AoC2019.Intcode

[<Test>]
let ``Day5 echo IO`` () =     
    let input () = 5
    let mutable out = -1
    let output x = 
        out <- x
    run (Program [3;0;4;0;99]) 0 input output |> ignore
    out |> should equal 5

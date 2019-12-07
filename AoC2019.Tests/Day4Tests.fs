module AoC2019.Day4.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``Day4 Example`` () =     
    let bounds = (100000, 999999)
    isCandidate bounds 111111 |> should equal true
    isCandidate bounds 223450 |> should equal false
    isCandidate bounds 123789 |> should equal false
    

let input = (108457, 562041)
[<Test>]
let ``Day4 A`` () =
    let (min, max) = input
    seq { min .. max } |> Seq.filter (isCandidate input) |> Seq.length |> should equal 2779
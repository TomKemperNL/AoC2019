module AoC2019.Day3.Tests

open NUnit.Framework
open FsUnit
open System.IO

[<Test>]
let ``Day3 Example`` () =     
    manhattan "R8,U5,L5,D3" "U7,R6,D4,L4" |> should equal 6
    manhattan "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" |> should equal 159
    manhattan "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |> should equal 135

[<Test>]
let ``Day3 B Example`` () =     
    steps "R8,U5,L5,D3" "U7,R6,D4,L4" |> should equal 30
    steps "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" |> should equal 610
    steps "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |> should equal 410

let input =     
    File.ReadAllLines("./Dag3InputA.txt")

[<Test>]
[<Explicit>]
let ``Day3 A`` () =     
    manhattan input.[0] input.[1] |> should equal 1674

[<Test>]
[<Explicit>]
let ``Day3 B`` () =     
    steps input.[0] input.[1] |> should equal 14012
    
module AoC2019.Day1.Tests

open NUnit.Framework
open FsUnit
open System.IO

let input =     
    File.ReadAllLines("./Dag1InputA.txt") |>
         Seq.map System.Int32.Parse

[<Test>]
let ``Mass Example`` () = 
    fuelrequired 12 |> should equal 2
    fuelrequired 14 |> should equal 2
    fuelrequired 1969 |> should equal 654
    fuelrequired 100756 |> should equal 33583

[<Test>]
let ``Part A`` () =
    Seq.sumBy fuelrequired input
        |> should equal 3471229

[<Test>]
let ``Mass Example B`` () = 
    fuelrequired2 14 |> should equal 2
    fuelrequired2 1969 |> should equal 966
    fuelrequired2 100756 |> should equal 50346

[<Test>]
let ``Part B`` () =    
    Seq.sumBy fuelrequired2 input
        |> should equal 5203967
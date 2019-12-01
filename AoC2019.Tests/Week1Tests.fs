module AoC2019.Week1.Tests

open NUnit.Framework
open FsUnit
open System.IO

[<Test>]
let ``Mass Example`` () = 
    fuelrequired 12 |> should equal 2
    fuelrequired 14 |> should equal 2
    fuelrequired 1969 |> should equal 654
    fuelrequired 100756 |> should equal 33583

[<Test>]
let ``Part A`` () =
    let lines = File.ReadAllLines("./Week1InputA.txt")
    lines 
        |> Seq.map (fun s -> s.Trim())
        |> Seq.map (fun s -> System.Int32.Parse s)
        |> Seq.map fuelrequired
        |> Seq.sum
        |> should equal 3471229

[<Test>]
let ``Mass Example B`` () = 
    fuelrequired2 14 |> should equal 2
    fuelrequired2 1969 |> should equal 966
    fuelrequired2 100756 |> should equal 50346

[<Test>]
let ``Part B`` () =
    let lines = File.ReadAllLines("./Week1InputA.txt")
    lines 
        |> Seq.map (fun s -> s.Trim())
        |> Seq.map (fun s -> System.Int32.Parse s)
        |> Seq.map fuelrequired2
        |> Seq.sum
        |> should equal 5203967
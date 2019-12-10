module AoC2019.Day6.Tests


open NUnit.Framework
open FsUnit
open System.IO

let parse = 
    Seq.map (fun (s: string) ->  s.Split(')')) >> Seq.map (fun [|a;b|] -> (OrbitObject a, OrbitObject b))


let input = 
    File.ReadAllLines "./Dag6Input.txt"
        |> parse


[<Test>]
let ``Day6 examples`` () =  
    let example = """COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"""
    let split = example.Split(System.Environment.NewLine)
    
    let map = createMap (parse split)
    System.Console.WriteLine(map)

    let huh = Map.find (OrbitObject "D") map
    huh |> should equal (OrbitObject "C")

    orbits map (OrbitObject "D") |> Set.count |> should equal 3
    orbits map (OrbitObject "L") |> Set.count |> should equal 7
    orbits map (OrbitObject "COM") |> Set.count |> should equal 0


    totals map |> should equal 42


[<Test>]
let ``Day6 A`` () = 
    let map = createMap input
    totals map |> should equal 147807

[<Test>]
let ``Day 6B Example`` () =
    let example = """COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"""
    let split = example.Split(System.Environment.NewLine)    
    let map = createMap (parse split)
    transfers map |> should equal 4

[<Test>]
let ``Day 6B`` () =
    let map = createMap input
    transfers map |> should equal 229
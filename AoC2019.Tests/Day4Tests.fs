module AoC2019.Day4.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``Day4 Example`` () =     
    let rules = [
        isInBounds (100000, 999999);
        isNDigits 6;
        hasTwoAdjacent;
        isIncreasing;
    ]
    
    isCandidate rules 111111 |> should equal true
    isCandidate rules 223450 |> should equal false
    isCandidate rules 123789 |> should equal false

[<Test>]
let ``Day4 A`` () =
    let (min, max) = (108457, 562041)

    let rules = [
        isInBounds (min, max);
        isNDigits 6;
        hasTwoAdjacent;
        isIncreasing;
    ]

    seq { min .. max } |> Seq.filter (isCandidate rules) |> Seq.length |> should equal 2779

[<Test>]
let ``Has Three Adjacent`` () =
    hasThreeAdjacent 112211 |> should equal false
    hasThreeAdjacent 111223 |> should equal true
    hasThreeAdjacent 112223 |> should equal true

[<Test>]
let ``Has Exactly two Adjacent`` () =
    hasExactlyTwoAdjacent 112233 |> should equal true
    hasExactlyTwoAdjacent 123444 |> should equal false
    hasExactlyTwoAdjacent 444123 |> should equal false
    hasExactlyTwoAdjacent 111122 |> should equal true

[<Test>]
let ``Can convert number to list`` () =
    toList 112211 |> should equivalent [1;1;2;2;1;1;]

[<Test>]
let ``Day4 B`` () =
    let (min, max) = (108457, 562041)

    let rules = [
        isInBounds (min, max);
        isNDigits 6;
        hasTwoAdjacent;
        isIncreasing;
        hasExactlyTwoAdjacent
    ]

    let result = seq { min .. max } |> Seq.filter (isCandidate rules) |> Seq.length     
    result |> should greaterThan 1444    
    result  |> should greaterThan 1847
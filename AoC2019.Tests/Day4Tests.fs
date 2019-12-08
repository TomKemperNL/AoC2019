module AoC2019.Day4.Tests

open NUnit.Framework
open FsUnit

open AoC2019.Shared

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
let ``Has Exactly two Adjacent`` () =
    hasExactlyTwoAdjacent 112233 |> should equal true
    hasExactlyTwoAdjacent 123444 |> should equal false
    hasExactlyTwoAdjacent 444123 |> should equal false    
    hasExactlyTwoAdjacent 111111 |> should equal false    
    hasExactlyTwoAdjacent 121111 |> should equal false
    hasExactlyTwoAdjacent 221111 |> should equal true
    hasExactlyTwoAdjacent 122111 |> should equal true
    hasExactlyTwoAdjacent 112211 |> should equal true
    hasExactlyTwoAdjacent 111221 |> should equal true
    hasExactlyTwoAdjacent 111122 |> should equal true

[<Test>]
let ``Get Exact Pairs`` () =
    getExactPairs (toList 112233) |> should equivalent [[1;1];[2;2;];[3;3;]]
    getExactPairs (toList 123444) |> should equivalent []
    getExactPairs (toList 111112) |> should equivalent []
    getExactPairs (toList 211111) |> should equivalent []
    getExactPairs (toList 111121) |> should equivalent []
    getExactPairs (toList 111221) |> should equivalent [[2;2;]]
    getExactPairs (toList 111122) |> should equivalent [[2;2;]]
    

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
    result |> should greaterThan 1847
    result |> should not' (equal 2028)
    result |> should equal 1972
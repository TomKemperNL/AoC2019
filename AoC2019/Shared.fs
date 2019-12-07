module AoC2019.Shared

let log message (x: 'a) =
    printfn "%s %O" message x
    x

let logMany message (xs: 'a seq) =
    let joined = String.concat "," (Seq.map (sprintf "%O") xs)
    log message joined |> ignore    
    xs

open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
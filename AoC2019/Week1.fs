module AoC2019.Week1
open System

let log message x =
    printfn "%s %O" message x
    x

let fuelrequired (m:int) =
    m   |> float          
        |> fun x -> x / 3.0        
        |> Math.Floor
        |> fun x -> x - 2.0        
        |> int

let rec fuelrequired2 (m:int) = 
    let fuelmass = fuelrequired m |> max 0     
    if fuelmass = 0 then
        fuelmass
    else
        fuelmass + fuelrequired2 fuelmass
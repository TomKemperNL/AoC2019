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
    let fuelForMass = fuelrequired m |> max 0 
    let requiredForFuel = fuelrequired fuelForMass |> max 0
    if requiredForFuel = 0 then
        fuelForMass
    else
        fuelForMass + requiredForFuel + fuelrequired2 requiredForFuel
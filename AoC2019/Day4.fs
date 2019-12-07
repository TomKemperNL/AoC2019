module AoC2019.Day4

type Bounds = int*int


let isCandidate (lower,upper) (x: int) =
    let isInBounds = (x >= lower) && (x <= upper)
    let isSixDigits = (x.ToString().Length = 6)
    let xAsList = x.ToString() |> Seq.map int |> Seq.toList
    let isIncreasing = (List.sort xAsList) = xAsList    
    let hasTwoAdjacent = 
        List.zip (List.skip 1 xAsList) (List.take 5 xAsList) 
            |> List.exists (fun (x,y) -> (x = y))        

    (isInBounds && isSixDigits && hasTwoAdjacent && isIncreasing)
    
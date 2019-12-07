module AoC2019.Day4

open AoC2019.Shared

type Bounds = int*int

let toList x = 
    x.ToString() |> Seq.map (string >> int) |> Seq.toList

let isInBounds (lower, upper) x = (x >= lower) && (x <= upper)
let isNDigits n x = (x.ToString().Length = n)
let isIncreasing x = 
    let xAsList = toList x
    (List.sort xAsList) = xAsList

let hasTwoAdjacent x = 
    let xAsList = toList x
    List.zip (List.skip 1 xAsList) (List.take 5 xAsList) 
        |> List.exists (fun (x,y) -> (x = y))
        

let hasThreeAdjacent x =
    let xAsList = toList x    
    let length = List.length xAsList

    let left = List.take (length - 2) xAsList
    let middle = (List.skip 1 >> List.take (length - 2)) xAsList 
    let right = List.skip 2 xAsList

    List.zip3 left middle right |> List.exists (fun (x,y,z) -> (x = y) && (y = z))

let hasExactlyTwoAdjacent x =
    let xAsList = toList x    
    let pairs = seq {
        let list = new System.Collections.Generic.List<int>()
        let mutable current = -1
        for i in xAsList do
            if i <> current then
                if list.Count >= 2 then
                    yield list
                list.Clear()
            else 
                if list.Count < 2 then
                    list.Add(i)
                else 
                    list.Clear()
            current <- i
            list.Add i
    }
    not <| Seq.isEmpty pairs
    

let isCandidate rules (x: int) =
    List.forall (fun r -> r x) rules 
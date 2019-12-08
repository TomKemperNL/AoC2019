module AoC2019.Day4

open AoC2019.Shared

type Bounds = int*int

let isInBounds (lower, upper) x = (x >= lower) && (x <= upper)
let isNDigits n x = (x.ToString().Length = n)
let isIncreasing x = 
    let xAsList = toList x
    (List.sort xAsList) = xAsList

let hasTwoAdjacent x = 
    let xAsList = toList x
    List.zip (List.skip 1 xAsList) (List.take 5 xAsList) 
        |> List.exists (fun (x,y) -> (x = y))        

let getExactPairs xAsList =     
    seq {
        let list = new System.Collections.Generic.List<int>()
        let mutable previous = -1        
        for i in xAsList do                        
            if i <> previous then
                if list.Count = 2 then                    
                    yield List.ofSeq list
                list.Clear()
                list.Add i
            else                
                list.Add i                
            previous <- i  

        if list.Count = 2 then            
            yield List.ofSeq list
    } 

let hasExactlyTwoAdjacent x =
    let xAsList = toList (x |> log "input:")  
    let pairs = getExactPairs xAsList
    log "wut: " (Seq.length pairs) |> ignore    
    Seq.iter (fun ps -> logMany "pairs" ps |> ignore) pairs
    not <| Seq.isEmpty pairs
    

let isCandidate rules (x: int) =
    List.forall (fun r -> r x) rules 
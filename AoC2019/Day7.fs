module AoC2019.Day7
open AoC2019.Intcode
type Thruster = System*int

let linkedIO setting = 
    let q = System.Collections.Generic.Queue<int>()
    q.Enqueue(setting)    
    let input () = 
        q.Dequeue()
    let output x = 
        q.Enqueue x
    (input, output)



let runThruster p (s1,s2,s3,s4,s5) =     
    let input1, output0 = linkedIO s1    
    let input2, output1 = linkedIO s2
    let input3, output2 = linkedIO s3
    let input4, output3 = linkedIO s4
    let input5, output4 = linkedIO s5

    let mutable output = -1

    let thruster1 = (p, input1, output1)
    let thruster2 = (p, input2, output2)
    let thruster3 = (p, input3, output3)
    let thruster4 = (p, input4, output4)
    let thruster5 = (p, input5, fun x -> output <- x)

    output0(0)
    run thruster1 |> ignore
    run thruster2 |> ignore
    run thruster3 |> ignore
    run thruster4 |> ignore
    run thruster5 |> ignore

    output


let toTuple5 list =
    match list with 
    | [x1;x2;x3;x4;x5] -> (x1,x2,x3,x4,x5)
    | _ -> failwith "not a 5-length list"

let toList (x1,x2,x3,x4,x5) =
    [x1;x2;x3;x4;x5]


let rec inAllSpots (item: 'a) (list: 'a list)= 
    match list with 
    | [] -> [[item]]
    | h::t ->            
        let opt = item::(h::t) 
        let others = List.map (fun o -> h::o) (inAllSpots item t)
        opt :: others

let rec permutations (items: 'a list) : 'a list list = 
    match items with 
    | [] -> [[]]
    | h :: t ->    
        let rest = permutations t
        let zoinks = List.map (inAllSpots h) rest //TODO lolwut
        List.concat zoinks
         
        

let maxThruster p = 
    let settings = [0;1;2;3;4]

    let allSettings = permutations settings
    let allSettings = List.map toTuple5 allSettings

    List.map (runThruster p) allSettings |> List.max
    
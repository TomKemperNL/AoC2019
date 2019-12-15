module AoC2019.Day7
open AoC2019.Intcode
open AoC2019.Shared

type ThrusterConfig = System*int


let linkedIO setting marker = 
    let q = System.Collections.Generic.Queue<int>()
    q.Enqueue(setting)

    let input () = 
        match q.TryPeek() with 
        | true, x -> 
            log (sprintf "Providing %s input:" marker) x |> ignore
            Some (q.Dequeue())
        | false, _ -> 
            None
        
    let output x =         
        log (sprintf "Providing %s output:" marker) x |> ignore
        q.Enqueue x
    
    (input, output)

let xcombine (configs: ((Input*Output)*Program) list) finalOutput : (Output * System list) =
    match configs with
    | [] -> (fun _ -> ()), []
    | ((i1,o0), _) :: _ ->           
        let rec combinePrograms configs ((prevIn, prevOut): Input*Output) (results: System list) = 
            match configs with 
            | [] -> 
                results |> List.rev
            | [((i, _), p)] ->                
                (p, i, finalOutput) :: results |> List.rev
            | ((i1,_), p1) :: (((i2,o1), p2) :: t) ->    
                let result = (p1, i1, o1)
                combinePrograms (((i2,o1), p2) :: t) (i2,o1) ((result :: results))            

        o0, combinePrograms configs (i1, o0) []

let rec runLoop (systems: SystemState list) =     
    match systems with
    | [] -> ()
    | (h,pos) :: t ->
        match (runAt pos h) with
        | End, _ ->             
            runLoop t
        | Pause cont, _ ->
            let atEnd = List.append t [cont]
            runLoop atEnd



let runThruster (p: Program) (settings: int list) = 
    let mutable outputResult = -1
    let finalOutput = fun x ->         
        outputResult <- x
    let settings = List.mapi (fun ix s -> linkedIO s (sprintf "s%i" (ix + 1))) settings

    let configs = Seq.initInfinite (constant p) |> Seq.zip settings |> List.ofSeq
    let (passInput, thruster) = xcombine configs finalOutput
    passInput 0

    let startingStates = Seq.zip thruster (Seq.initInfinite (constant 0)) |> List.ofSeq

    //List.iter (run >> ignore) thruster
    runLoop startingStates |> ignore
    
    outputResult

let rec permutations (items: 'a list) : 'a list list =     
    let rec inAllSpots (item: 'a) (list: 'a list)= 
        match list with 
        | [] -> [[item]]
        | h::t ->            
            let opt = item::(h::t) 
            let others = List.map (fun o -> h::o) (inAllSpots item t)
            opt :: others
    

    match items with 
    | [] -> [[]]
    | h :: t ->    
        let rest = permutations t
        let zoinks = List.map (inAllSpots h) rest //TODO lolwut
        List.concat zoinks
         
        

let maxThruster p = 
    let settings = [0;1;2;3;4]
    let allSettings = permutations settings
    List.map (runThruster p) allSettings |> List.max
   
let maxThrusterLoop p = 
    let settings = [5;6;7;8;9]
    let allSettings = permutations settings
    List.map (runThruster p) allSettings |> List.max
    
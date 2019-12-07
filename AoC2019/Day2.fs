module AoC2019.Day2

open AoC2019.Shared
open AoC2019.Intcode

let run (Intcode input) pos =     
    let inputArray = Array.ofList input
    let set = Array.set inputArray
    let get = Array.get inputArray
    let op pos = 
        let op =  mapping.[get pos]
        let arity = arity.[op]
        if arity > 0 then        
            (op, Array.sub inputArray (pos + 1) arity, pos + 1 + arity)
        else 
            (op, [||], pos)

    let rec runArray input pos = 
        match op pos with
        | (Halt, [||], _) -> input
        | (Add, [|x;y;p|], next) ->
            set p (get x + get y) |> ignore
            runArray input next
        | (Multiply, [|x;y;p|], next) ->
            set p (get x * get y) |> ignore
            runArray input next
        | (op, args, next) ->        
            sprintf "Unknown operator: %O on %s" op (System.String.Join(',', args))|> failwith

    runArray inputArray pos |> List.ofArray |> Intcode

let runValue (Intcode input) noun verb =    
    let rest = List.skip 3 input
    let [a;_;_] = List.take 3 input
    let program = Intcode (List.append [a;noun;verb] rest)
    let (Intcode result) = run program 0 
    List.head result

let bruteForce intcode desired =    
    let options = seq {
        for n in 0..100 do
            for v in 0..100 do
                yield (n,v)
    }

    let isMatch (n,v) =         
        try
            let result = runValue intcode n v           
            result = desired
        with
        | _ -> false

    Seq.tryFind isMatch options
            
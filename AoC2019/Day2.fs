module AoC2019.Day2
open AoC2019.Shared

type Intcode = Intcode of int array
type Opcode = 
    | Halt
    | Add
    | Multiply

let mapping = dict [
    (99, Halt);
    (1, Add);
    (2, Multiply)]

let arity = dict [
    (Halt, 0);
    (Add, 3);
    (Multiply, 3)
]

let rec run (Intcode input) pos =     
    let set = Array.set input
    let get = Array.get input
    let op x = 
        let op =  mapping.[get pos]
        let arity = arity.[op]
        if arity > 0 then        
            (op, Array.sub input (pos + 1) arity, pos + 1 + arity)
        else 
            (op, [||], pos)

    match op pos with
    | (Halt, [||], _) -> (Intcode input)
    | (Add, [|x;y;p|], next) ->
        set p (get x + get y) |> ignore
        run (Intcode input) next
    | (Multiply, [|x;y;p|], next) ->
        set p (get x * get y) |> ignore
        run (Intcode input) next
    | (op, args, next) ->        
        sprintf "Unknown operator: %O on %s" op (System.String.Join(',', args))|> failwith

let runValue (Intcode input) noun verb =    
    Array.set input 1 noun
    Array.set input 2 verb
    run (Array.copy input |> Intcode) 0 |> fun (Intcode x) -> x |> Array.head

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
            
module AoC2019.Intcode

type Input = unit -> int
type Output = int -> unit
type Program = Program of int list

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

let run (Program input) pos =
    let inputArray = Array.ofList input
    let set = Array.set inputArray
    let get = Array.get inputArray
    let parseOpcode pos = 
        let op =  mapping.[get pos]
        let arity = arity.[op]
        if arity > 0 then        
            (op, Array.sub inputArray (pos + 1) arity, pos + 1 + arity)
        else 
            (op, [||], pos)

    let rec runArray input pos = 
        match parseOpcode pos with
        | (Halt, [||], _) -> input
        | (Add, [|x;y;p|], next) ->
            set p (get x + get y) |> ignore
            runArray input next
        | (Multiply, [|x;y;p|], next) ->
            set p (get x * get y) |> ignore
            runArray input next
        | (op, args, next) ->        
            sprintf "Unknown operator: %O on %s" op (System.String.Join(',', args))|> failwith

    runArray inputArray pos |> List.ofArray |> Program

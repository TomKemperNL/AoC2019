module AoC2019.Intcode

type Input = unit -> int
type Output = int -> unit
type Program = Program of int list

type Opcode = 
    | Halt
    | Add
    | Multiply
    | Input
    | Output
    
let mapping = dict [
    (99, Halt);
    (1, Add);
    (2, Multiply);
    (3, Input);
    (4, Output);
]

let arity = dict [
    (Halt, 0);
    (Add, 3);
    (Multiply, 3);
    (Input, 1);
    (Output, 1)
]

let noInput = fun() -> failwith "No Input Configured"
let noOutput = fun x -> failwith "No Output Configured"

let run (Program ints) (pos: int) (input: Input) (output: Output) =
    let inputArray = Array.ofList ints
    let set = Array.set inputArray
    let get = Array.get inputArray
    let parseOpcode x = 
        let op =  mapping.[get x]
        let arity = arity.[op]
        if arity > 0 then        
            (op, Array.sub inputArray (x + 1) arity, x + 1 + arity)
        else 
            (op, [||], x)

    let rec runArray inputArray pos = 
        match parseOpcode pos with
        | (Halt, [||], _) -> inputArray
        | (Add, [|x;y;p|], next) ->
            set p (get x + get y) |> ignore
            runArray inputArray next
        | (Multiply, [|x;y;p|], next) ->
            set p (get x * get y) |> ignore
            runArray inputArray next
        | (Input, [|p|], next) ->
            set p (input()) |> ignore
            runArray inputArray next
        | (Output, [|p|], next) ->
            output (get p)
            runArray inputArray next
        | (op, args, next) ->        
            sprintf "Unknown operator: %O on %s" op (System.String.Join(',', args))|> failwith

    runArray inputArray pos |> List.ofArray |> Program

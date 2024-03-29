﻿module AoC2019.Intcode

open AoC2019.Shared

type Input = unit -> int option
module Input = 
    let constant x = 
        fun () -> Some x
type Output = int -> unit

type Program = Program of int list
type System = Program*Input*Output
type SystemState = (Program*Input*Output) * int * int

type ProgramResult = 
    | End 
    | Pause of SystemState




type ParameterMode =
    | Position
    | Immediate
    | Relative

module ParameterMode =
    let fromInt x =
        match x with
        | 0 -> Position
        | 1 -> Immediate
        | 2 -> Relative
        | n -> failwith <| sprintf "Unknown parameter mode %d" n

type Parameter = ParameterMode * int

type Opcode = 
    | Halt
    | Add
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | AdjustBase

module Opcode = 
    let fromList xs =
        match xs with 
        | [9;9] -> Halt
        | [0;1] -> Add
        | [0;2] -> Multiply
        | [0;3] -> Input
        | [0;4] -> Output
        | [0;5] -> JumpIfTrue
        | [0;6] -> JumpIfFalse
        | [0;7] -> LessThan
        | [0;8] -> Equals
        | [0;9] -> AdjustBase
        | xs -> failwith <| sprintf "Unknown opcode %O" xs

    let arity op =
        match op with
        | Halt -> 0        
        | Add | Multiply | LessThan | Equals -> 3        
        | JumpIfTrue | JumpIfFalse -> 2
        | Input | Output | AdjustBase -> 1    

let noInput = fun() -> failwith "No Input Configured"
let noOutput = fun x -> failwith "No Output Configured"

let parseInstruction x =     
    let items = toList x 
    let items = if items.Length > 1 then items else 0::items //Backwards compatibility with day2, where instructions might be 1 length
    let paramModes, operator = List.splitAt ((List.length items) - 2) items    
    let op = Opcode.fromList operator
    let paramModes = paramModes |> Seq.rev 
    let paramModes = Seq.append paramModes (Seq.initInfinite (fun _ -> 0))
    let paramArity = Opcode.arity op
    let paramModes = Seq.take paramArity paramModes |> List.ofSeq
    let result = (op, List.map ParameterMode.fromInt paramModes)
    result



let runAt relativeBase pos ((Program ints),input,output) : ProgramResult * Program =
    let buffer = Seq.initInfinite (constant 0)
    let length = List.length ints
    let inputArray = Seq.append ints buffer |> Seq.take (length * 10) |> Array.ofSeq   
    

    let set p v =     
        Array.set inputArray p v
    let get = Array.get inputArray

    let evaluateParam relativeBase (item, paramMode) =
        match paramMode with
        | Position -> get item
        | Immediate -> item
        | Relative -> get (item + relativeBase)

    let parseOpcode x = 
        let (op, paramModes) =  parseInstruction (get x)
        let arity = Opcode.arity op
        if arity > 0 then
            let items = Array.sub inputArray (x + 1) arity |> List.ofArray            
            let parameters = List.zip items paramModes

            (op, parameters, x + 1 + arity)
        else 
            (op, [], x)

    let rec runArray inputArray pos relativeBase : (ProgramResult * int array)= 
        let evaluateParam = evaluateParam relativeBase

        match parseOpcode pos with
        | (Halt, [], _) -> 
            (End, inputArray |> log "Halting") 
        | (Add, [(x,xmode);(y,ymode);(p,pmode)], next) ->
            set p ((evaluateParam (x, xmode)) + (evaluateParam (y, ymode))) |> ignore
            runArray inputArray next relativeBase
        | (Multiply, [(x,xmode);(y,ymode);(p,pmode)], next) ->
            set p ((evaluateParam (x, xmode))  * (evaluateParam (y, ymode))) |> ignore
            runArray inputArray next relativeBase
        | (Input, [(p, pmode)], next) ->

            match input() with
            | Some x ->
                set p x |> ignore            
                runArray inputArray next relativeBase
            | None -> 
                let program = Program (List.ofArray inputArray)
                let system = (program, input, output), pos, relativeBase
                (Pause system, inputArray) |> log "Pausing"
        | (Output, [(p, pmode)], next) ->
            output (evaluateParam (p, pmode))
            runArray inputArray next relativeBase
        | (JumpIfTrue, [(test, testMode); (p, pmode)], next) ->
            if evaluateParam (test,testMode) <> 0 then
                runArray inputArray (evaluateParam (p,pmode)) relativeBase
            else
                runArray inputArray next relativeBase
        | (JumpIfFalse, [(test, testMode); (p, pmode)], next) ->
            if evaluateParam (test,testMode) = 0 then
                runArray inputArray (evaluateParam (p,pmode)) relativeBase
            else
                runArray inputArray next relativeBase
        | (LessThan, [(x,xmode);(y,ymode);(p,pmode)], next) ->
            if evaluateParam (x,xmode) < evaluateParam (y, ymode) then
                set p 1                
            else
                set p 0
                    
            runArray inputArray next relativeBase
        | (Equals, [(x,xmode);(y,ymode);(p,pmode)], next) ->
            if evaluateParam (x,xmode) = evaluateParam (y, ymode) then
                set p 1                
            else
                set p 0                
            runArray inputArray next relativeBase
        | (AdjustBase, [(p, pmode)], next) ->
            let adjustment = evaluateParam (p, pmode)
            runArray inputArray next (relativeBase + adjustment)
        | (op, args, next) ->        
            sprintf "Unknown operator: %O on %s" op (System.String.Join(',', args))|> failwith

    let (t, p) = runArray inputArray pos relativeBase
    (t, Program (List.ofArray p))

let run (system:System) : ProgramResult * Program =    
    runAt 0 0 system
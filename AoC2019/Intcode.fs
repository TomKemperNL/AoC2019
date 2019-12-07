module AoC2019.Intcode

type Intcode = Intcode of int list
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
module AoC2019.Day2

open AoC2019.Shared
open AoC2019.Intcode

let runValue (Program input) noun verb =    
    let rest = List.skip 3 input
    let [a;_;_] = List.take 3 input
    let program = Program (List.append [a;noun;verb] rest)
    let (Program result) = run program 0 noInput noOutput
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
            
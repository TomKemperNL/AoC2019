module AoC2019.Day2

let rec run (input: int array) pos = 
    let get = Array.get input
    let set = Array.set input

    match get pos with
    | 99 -> input
    | _ ->
        let items = Array.sub input pos 4        
        match items with
        | [|1; x;y;p|] ->
            set p (get x + get y) |> ignore
            run input (pos+4)
        | [|2; x;y;p|] ->
            set p (get x * get y) |> ignore
            run input (pos+4)
        | _ -> failwith "Nope"
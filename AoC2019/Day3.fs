module AoC2019.Day3
open AoC2019.Shared

type Coord = (int*int)

type Instruction = 
    | U of int
    | D of int
    | R of int
    | L of int

let parse (line: string) =
    let parseInstruction instruction = 
        match instruction with 
        | Regex "U(\d+)" [d] -> 
            System.Int32.Parse d |> U
        | Regex "D(\d+)" [d] -> 
            System.Int32.Parse d |> D
        | Regex "L(\d+)" [d] -> 
            System.Int32.Parse d |> L
        | Regex "R(\d+)" [d] -> 
            System.Int32.Parse d |> R
        | _ -> failwith (sprintf "Unknown instruction %s" instruction)

    Seq.map parseInstruction (line.Split(','))
    

let toCoordinates instructions origin =
    let instructionToCoordinates (ps, (x,y)) instruction =
        let (op, nr) = match instruction with
        | L nr -> 
            (fun s -> (x - s, y)), nr
        | R nr -> 
            (fun s -> (x + s, y)), nr
        | U nr -> 
            (fun s -> (x, y + s)), nr
        | D nr -> 
            (fun s -> (x, y - s)), nr

        let points = seq { 1 .. nr} |> Seq.map op            
        (Seq.concat [ps; points], op nr)
    
    Seq.fold instructionToCoordinates origin instructions


let manhattan line1 line2 =
    let l1 = toCoordinates (parse line1) ([],(0,0)) |> fst |> Set.ofSeq 
    let l2 = toCoordinates (parse line2) ([],(0,0)) |> fst |> Set.ofSeq
    Set.intersect l1 l2         
        |> Set.map (fun (x,y) -> System.Math.Abs x + System.Math.Abs y)
        |> Set.minElement

let steps line1 line2 =
    let l1 = toCoordinates (parse line1) ([],(0,0)) |> fst |> List.ofSeq
    let l2 = toCoordinates (parse line2) ([],(0,0)) |> fst |> List.ofSeq

    let findSpot p = List.findIndex ((=) p)

    Set.intersect (Set.ofList l1) (Set.ofList l2) 
        |> Set.map (fun p -> findSpot p l1 + findSpot p l2 + 2)
        |> Set.minElement
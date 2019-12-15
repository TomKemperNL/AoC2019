module AoC2019.Day8

open AoC2019.Shared

type Layer = int array
type Image = Layer list

let private splitInParts initial partLength input =
    seq {
        let items = Array.init (partLength) (constant initial)
        let mutable count = 0
        for x in input do
            Array.set items count x
            if count = (items.Length - 1) then
                yield Array.copy items
                count <- 0
            else
                count <- count + 1
    }

let splitInLayers width height (input: int seq) : Image = 
    splitInParts 0 (width * height) input |> List.ofSeq

let rec flatten (image: Image) : Layer = 
    let pickColor ((upper, lower): int*int) = 
        match upper with
        | 0 | 1 -> 
            upper
        | 2 -> 
            lower
        | _ -> 
            failwith <| sprintf "Unknown color %i" upper

    match image with    
    | [] -> Array.empty
    | [x] -> x
    | h :: t ->
        let layer = flatten t
        Array.zip h layer |> Array.map pickColor
        
let print width (layer: Layer) : unit = 
    let mapCharacter x = 
        match x with 
        | 0 -> "   "
        | 1 -> " X "
        | 2 -> "   "
        | _ -> " ? "

    let printLine (line: string array) : unit =
        System.String.Join("", line) |> printfn "%s" |> ignore

    let layer = Array.map mapCharacter layer
    let lines = splitInParts " " width layer 
    Seq.iter printLine lines
    
let leastZerosLayer (layers: Image) = 
    let zerosInLayer layer = 
        Seq.filter (fun x -> x = 0) layer |> Seq.length

    List.minBy zerosInLayer layers
    
let multiplyLayer (layer: Layer) = 
    let gather (ones, twos) item = 
        match item with 
        | 1 -> (ones, twos + 1)
        | 2 -> (ones + 1, twos)
        | _ -> (ones, twos)

    let (ones, twos) = Array.fold gather (0,0) layer
    ones * twos



let checksum width height (input: int seq) =
    let layers = splitInLayers width height input
    let layer = leastZerosLayer layers
    multiplyLayer layer
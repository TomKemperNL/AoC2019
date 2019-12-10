module AoC2019.Day6

type OrbitObject =
    OrbitObject of string

type OrbitMap = 
    Map<OrbitObject, Set<OrbitObject>>

let createReverseMap pairs =    
    let updateMap map (center,satelite) =         
        match Map.tryFind center map with
        | Some orbits ->             
            Map.add center (Set.add satelite orbits) map
        | None -> 
            Map.add center (Set.singleton satelite) map            
    Seq.fold updateMap Map.empty pairs 


let createMap pairs =  
    let flip (a,b) = (b,a)
    Seq.map flip pairs |> Map.ofSeq 
    
let memoize fn =
  let cache = new System.Collections.Generic.Dictionary<_,_>()
  (fun x ->
    match cache.TryGetValue x with
    | true, v -> v
    | false, _ -> let v = fn (x)
                  cache.Add(x,v)
                  v)

let rec isOrbitedBy map o =
    let directOrbits = Map.tryFind o map
    match directOrbits with 
    | Some directOrbits ->
        let indirectOrbits = Set.unionMany (Set.map (isOrbitedBy map) directOrbits)
        Set.union directOrbits indirectOrbits
    | None -> Set.empty

let rec orbits map o = 
    let center = Map.tryFind o map
    match center with 
    | Some center ->
        Set.add center (orbits map center)
    | None ->
        Set.empty

let totals map =
    Map.toSeq map |> Seq.sumBy (fun (k, v) -> orbits map k |> Set.count)

let transfers map = 
    let orbitsYou = orbits map (OrbitObject "YOU")
    let orbitsSan = orbits map (OrbitObject "SAN")
    let intersect = Set.intersect orbitsYou orbitsSan
    let union = Set.union orbitsYou orbitsSan
    let diff = Set.difference union intersect
    Set.count diff




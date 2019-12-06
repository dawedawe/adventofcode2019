namespace Adventofcode

module Day6 =

    [<Literal>]
    let InputFile = "Day6Input.txt"

    type Orbiter = {
        Name : string
        DirectOrbits : int
        IndirectOrbits : int
    }

    let parseLine (s : string) = 
        let i = s.IndexOf(")")
        (s.Substring(0, i), s.Substring(i + 1))

    let getOrbits path =
        System.IO.File.ReadAllLines path
        |> Array.map parseLine

    let hopsToCom (orbits : (string * string) []) name =
        let mutable hops = 0
        let mutable hop = Array.tryFind (fun (x, y) -> y = name) orbits
        while Option.isSome hop do
            hops <- hops + 1
            hop <- Array.tryFind (fun (x, y) -> y = fst hop.Value) orbits
        hops

    let pathToCom (orbits : (string * string) []) name =
        let mutable path = Array.empty
        let mutable hop = Array.tryFind (fun (x, y) -> y = name) orbits
        while Option.isSome hop do
            path <- Array.append path [| fst hop.Value |]
            hop <- Array.tryFind (fun (_, y) -> y = fst hop.Value) orbits
        path

    let buildMap (orbits : (string * string) []) =
        let names1 = Array.map fst orbits
        let names2 = Array.map snd orbits
        let names = Array.append names1 names2 |> Array.distinct
        let mutable orbitMap = Array.empty
        for name in names do
            let hops = hopsToCom orbits name
            match hops with
            | 0 -> orbitMap <- Array.append orbitMap [| { Name = name; DirectOrbits = 0; IndirectOrbits = 0 } |]
            | n -> orbitMap <- Array.append orbitMap [| { Name = name; DirectOrbits = 1; IndirectOrbits = n - 1 } |]
        orbitMap

    let day6 () =
        let orbits = getOrbits InputFile
        let orbitMap = buildMap orbits
        Array.sumBy (fun o -> o.IndirectOrbits + o.DirectOrbits) orbitMap

    let day6Part2 () =
        let orbits = getOrbits InputFile
        let pathOfYou = pathToCom orbits "YOU"
        let pathOfSan = pathToCom orbits "SAN"
        let mutable crossPoint = None
        let mutable i = 0
        while Option.isNone crossPoint && i < pathOfYou.Length do
            if Array.contains pathOfYou.[i] pathOfSan
            then crossPoint <- Some pathOfYou.[i]
            else i <- i + 1
        let pathOfYouToCross = Array.takeWhile (fun p -> p <> crossPoint.Value) pathOfYou
        let pathOfSanToCross = Array.takeWhile (fun p -> p <> crossPoint.Value) pathOfSan
        pathOfYouToCross.Length + pathOfSanToCross.Length

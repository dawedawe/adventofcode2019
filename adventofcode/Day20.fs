namespace Adventofcode

module Day20 =

    [<Literal>]
    let InputFile = "Day20Input.txt"

    type Position = { X: int; Y: int }

    type Path = {
        Pos: Position
        Symbol: string
        Distance: int
        Track: Position list
    }

    let getAdjacentPositions (pos : Position) =
        let northPos = { pos with Y = pos.Y - 1 }
        let southPos = { pos with Y = pos.Y + 1 }
        let westPos  = { pos with X = pos.X - 1 }
        let eastPos  = { pos with X = pos.X + 1 }
        [ northPos; southPos; westPos; eastPos; ]

    let isPortal s = String.length s = 2 && System.Char.IsUpper s.[0] && System.Char.IsUpper s.[1]
    
    type Portal = {
        Name: string
        A: Position
        B: Position
    }

    let constructPortalMap (portals: (string * Position) list) =
        portals
        |> List.filter (fun (name, _) -> name <> "ZZ" && name <> "AA")
        |> List.groupBy (fun (name, _) -> name)
        |> List.map (fun (name, group) -> (name, { Name = name; A = snd group.[0]; B = snd group.[1] }))
        |> Map.ofList

    let isUpperLetter c =
        ['A' .. 'Z'] |> List.contains c

    let rec markPortalPositions (grid: Map<Position, string>) (portalEntriesExits: (string * Position) list) =
        match portalEntriesExits with
        | [] -> grid
        | (name, pos)::xs -> let left = { pos with X = pos.X - 1}
                             let right = { pos with X = pos.X + 1}
                             let upper = { pos with Y = pos.Y - 1}
                             let lower = { pos with Y = pos.Y + 1}
                             if (grid.ContainsKey left && isUpperLetter grid.[left].[0])
                             then
                                printfn "fixing %A to %s" grid.[left] name
                                let grid' = Map.add left name grid
                                markPortalPositions grid' xs
                             else if (grid.ContainsKey right && isUpperLetter grid.[right].[0])
                             then
                                printfn "fixing %A to %s" grid.[right] name
                                let grid' = Map.add right name grid
                                markPortalPositions grid' xs
                             else if (grid.ContainsKey upper && isUpperLetter grid.[upper].[0])
                             then
                                printfn "fixing %A to %s" grid.[upper] name
                                let grid' = Map.add upper name grid
                                markPortalPositions grid' xs
                             else if (grid.ContainsKey lower && isUpperLetter grid.[lower].[0])
                             then
                                printfn "fixing %A to %s" grid.[lower] name
                                let grid' = Map.add lower name grid
                                markPortalPositions grid' xs
                             else
                                System.Exception("can't mark portal positions") |> raise

    let getAdjacentPortalPos (grid : Map<Position, string>) pos =
        let left = { pos with X = pos.X - 1}
        let right = { pos with X = pos.X + 1}
        let upper = { pos with Y = pos.Y - 1}
        let lower = { pos with Y = pos.Y + 1}
        if (grid.ContainsKey left && isPortal grid.[left])
        then left
        else if (grid.ContainsKey right && isPortal grid.[right])
        then right
        else if (grid.ContainsKey upper && isPortal grid.[upper])
        then upper
        else if (grid.ContainsKey lower && isPortal grid.[lower])
        then lower
        else System.Exception("can't locate adjacent portal pos") |> raise

    let getGridAndPortals path =
        let lines = System.IO.File.ReadAllLines path
        let mutable portalEntriesExits = List.empty
        let mutable grid = Map.empty

        for y in [0 .. lines.Length - 1] do
            let symbols = lines.[y].ToCharArray()
            for x in [0 .. symbols.Length - 1] do

                if System.Char.IsUpper(symbols.[x]) && x+1 < symbols.Length && System.Char.IsUpper(symbols.[x+1])
                then
                    let portalEntryExit = if x-1 > 0 && symbols.[x-1] = '.' then { X = x-1; Y = y } else { X = x+2; Y = y }
                    let portal = string symbols.[x] + string symbols.[x+1]
                    printfn "found portal %s (%A)" portal portalEntryExit
                    portalEntriesExits <- portalEntriesExits @ [portal, portalEntryExit]
                    
                else if System.Char.IsUpper(symbols.[x]) && y+1 < lines.Length && System.Char.IsUpper(lines.[y+1].[x])
                then
                    let portalEntryExit = if y-1 > 0 && lines.[y-1].[x] = '.' then { X = x; Y = y-1 } else { X = x; Y = y+2 }
                    let portal = string lines.[y].[x] + string lines.[y+1].[x]
                    printfn "found portal %s (%A)" portal portalEntryExit
                    portalEntriesExits <- portalEntriesExits @ [portal, portalEntryExit]
                
                grid <- grid.Add ({ X = x; Y = y}, string symbols.[x])
        let aaStart = portalEntriesExits |> List.find (fun (name, _) -> name = "AA") |> snd
        let zzStart = portalEntriesExits |> List.find (fun (name, _) -> name = "ZZ") |> snd
        let portalMap = constructPortalMap portalEntriesExits
        let grid' = markPortalPositions grid portalEntriesExits
        (grid', portalMap, aaStart, zzStart)

    let getMinDistance (grid: Map<Position, string>) (portalMap: Map<string, Portal>) source target =
        let startPath = { Pos = source; Symbol = grid.[source]; Distance = 0; Track = [source] }
        let queue = System.Collections.Generic.Queue<Path>()
        let mutable foundPath = List.empty

        queue.Enqueue startPath
        while (queue.Count <> 0) do
            let currentPath = queue.Peek()
            queue.Dequeue() |> ignore

            let adjacentPositions = getAdjacentPositions currentPath.Pos
            for adjacentPos in adjacentPositions do
                if grid.ContainsKey adjacentPos &&
                    grid.[adjacentPos] <> "#" &&
                    grid.[adjacentPos] <> "AA" &&
                    not (currentPath.Track |> List.contains adjacentPos)
                then
                    let symbol = grid.[adjacentPos]
                    let newDistance = currentPath.Distance + 1
                    if adjacentPos = target
                    then
                        let newTrack = currentPath.Track @ [adjacentPos]
                        foundPath <- foundPath @ [{ Pos = adjacentPos; Symbol = symbol; Distance = newDistance; Track = newTrack }]
                    else
                        let newPath = if symbol = "."
                                      then
                                        let newTrack = currentPath.Track @ [adjacentPos]
                                        { Pos = adjacentPos; Symbol = symbol; Distance = newDistance; Track = newTrack }
                                      else if isPortal symbol
                                      then
                                         let portal = portalMap.[symbol]
                                         printfn "passing portal %s" portal.Name
                                         let otherSide = if portal.A = currentPath.Pos then portal.B else portal.A
                                         let adjacentPortalPos = getAdjacentPortalPos grid otherSide
                                         let newTrack = currentPath.Track @ [adjacentPos; adjacentPortalPos; otherSide]
                                         { Pos = otherSide; Symbol = symbol; Distance = newDistance; Track = newTrack }
                                      else
                                        printfn "symbol |%s| %A" symbol adjacentPos
                                        System.Exception("bad") |> raise
                        queue.Enqueue newPath
        foundPath |> List.minBy (fun p -> p.Distance)

    let day20 () =
        let (grid, portalMap, aaStart, zzStart) = getGridAndPortals InputFile
        let d = getMinDistance grid portalMap aaStart zzStart
        d.Distance
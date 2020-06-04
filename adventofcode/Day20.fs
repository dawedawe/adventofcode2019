namespace Adventofcode

module Day20 =

    [<Literal>]
    let InputFile = "Day20Input.txt"

    type Position = { X: int; Y: int; L: int }

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
    
    type Direction =
    | Up
    | Down

    type Portal = {
        Name: string
        Apos: Position
        Adir: Direction
        Bpos: Position
        Bdir: Direction
    }

    let determinePortalDirection maxX maxY (portalEntryExit: Position) =
        let isOuter = portalEntryExit.X - 2 = 0 ||
                        portalEntryExit.Y - 2 = 0 ||
                        portalEntryExit.X + 2 = maxX ||
                        portalEntryExit.Y + 2 = maxY
        if isOuter then Up else Down

    let constructPortalMap (portals: (string * Position * Direction) list) =
        portals
        |> List.filter (fun (name, _, _) -> name <> "ZZ" && name <> "AA")
        |> List.groupBy (fun (name, _, _) -> name)
        |> List.map (fun (name, group) ->
            let (_, g0snd, g0thrd) = group.[0]
            let (_, g1snd, g1thrd) = group.[1]
            (name, { Name = name; Apos = g0snd; Adir = g0thrd; Bpos = g1snd; Bdir = g1thrd } ))
        |> Map.ofList

    let isUpperLetter c =
        ['A' .. 'Z'] |> List.contains c

    let rec markPortalPositions (grid: Map<Position, string>) (portalEntriesExits: (string * Position * Direction) list) =
        match portalEntriesExits with
        | [] -> grid
        | (name, pos, _)::xs -> let left = { pos with X = pos.X - 1}
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
        let maxY = lines.Length - 1
        let maxX = lines.[0].ToCharArray().Length - 1

        for y in [0 .. maxY] do
            let symbols = lines.[y].ToCharArray()
            for x in [0 .. maxX] do
                let portal = if System.Char.IsUpper(symbols.[x]) && x+1 < symbols.Length && System.Char.IsUpper(symbols.[x+1])
                             then
                                let portalName = string symbols.[x] + string symbols.[x+1]
                                let portalEntryExit = if x-1 > 0 && symbols.[x-1] = '.'
                                                      then { X = x-1; Y = y; L = 0; }
                                                      else { X = x+2; Y = y; L = 0; }
                                let portalDir = determinePortalDirection maxX maxY portalEntryExit
                                Some (portalName, portalEntryExit, portalDir)
                             else if System.Char.IsUpper(symbols.[x]) && y+1 < lines.Length && System.Char.IsUpper(lines.[y+1].[x])
                             then
                                let portalName = string lines.[y].[x] + string lines.[y+1].[x]
                                let portalEntryExit = if y-1 > 0 && lines.[y-1].[x] = '.'
                                                      then { X = x; Y = y-1; L = 0; }
                                                      else { X = x; Y = y+2; L = 0; }
                                let portalDir = determinePortalDirection maxX maxY portalEntryExit
                                Some (portalName, portalEntryExit, portalDir)
                             else
                                None
                if (Option.isSome portal)
                then
                    let (portalName, portalEntryExit, portalDir) = portal.Value
                    printfn "found portal %s (%A) %A" portalName portalEntryExit portalDir
                    portalEntriesExits <- portalEntriesExits @ [ (portalName, portalEntryExit, portalDir) ]
                
                grid <- grid.Add ({ X = x; Y = y; L = 0; }, string symbols.[x])
        let aaStart = portalEntriesExits |> List.find (fun (name, _, _) -> name = "AA") |> fun (_, pos, _) -> pos
        let zzStart = portalEntriesExits |> List.find (fun (name, _, _) -> name = "ZZ") |> fun (_, pos, _) -> pos
        let portalMap = constructPortalMap portalEntriesExits
        let grid' = markPortalPositions grid portalEntriesExits
        (grid', portalMap, aaStart, zzStart)

    let isLegalNextPosition grid visited pos =
        grid |> Map.containsKey pos &&
        grid.[pos] <> "#" &&
        grid.[pos] <> "AA" &&
        not (visited |> List.contains pos)

    let getMinDistance (grid: Map<Position, string>) (portalMap: Map<string, Portal>) source target =
        let startPath = { Pos = source; Symbol = grid.[source]; Distance = 0; Track = [source] }
        let queue = System.Collections.Generic.Queue<Path>()
        let mutable foundPath = None

        queue.Enqueue startPath
        while (queue.Count <> 0 && Option.isNone foundPath) do
            let currentPath = queue.Peek()
            queue.Dequeue() |> ignore

            let adjacentPositions = getAdjacentPositions currentPath.Pos
                                    |> List.filter (isLegalNextPosition grid currentPath.Track)
            for adjacentPos in adjacentPositions do
                let symbol = grid.[adjacentPos]
                let newDistance = currentPath.Distance + 1
                if adjacentPos = target
                then
                    let newTrack = currentPath.Track @ [adjacentPos]
                    foundPath <- Some { Pos = adjacentPos; Symbol = symbol; Distance = newDistance; Track = newTrack }
                else
                    let newPath = if symbol = "."
                                  then
                                    let newTrack = currentPath.Track @ [adjacentPos]
                                    { Pos = adjacentPos; Symbol = symbol; Distance = newDistance; Track = newTrack }
                                  else if isPortal symbol
                                  then
                                     let portal = portalMap.[symbol]
                                     printfn "passing portal %s" portal.Name
                                     let otherSide = if portal.Apos = currentPath.Pos then portal.Bpos else portal.Apos
                                     let adjacentPortalPos = getAdjacentPortalPos grid otherSide
                                     let newTrack = currentPath.Track @ [adjacentPos; adjacentPortalPos; otherSide]
                                     { Pos = otherSide; Symbol = symbol; Distance = newDistance; Track = newTrack }
                                  else
                                    printfn "symbol |%s| %A" symbol adjacentPos
                                    System.Exception("bad") |> raise
                    queue.Enqueue newPath
        foundPath.Value

    let day20 () =
        let (grid, portalMap, aaStart, zzStart) = getGridAndPortals InputFile
        let d = getMinDistance grid portalMap aaStart zzStart
        d.Distance

    let isLegalNextPositionPart2 grid (portalMap: Map<string, Portal>) visited currentPos posCandidate =
        let posCandidateWithLevel0 = { posCandidate with L = 0 }
        let conditions1 = grid |> Map.containsKey posCandidateWithLevel0 &&
                          grid.[posCandidateWithLevel0] <> "#" &&
                          grid.[posCandidateWithLevel0] <> "AA" &&
                          not (visited |> List.contains posCandidate)
        
        if conditions1 && isPortal grid.[posCandidateWithLevel0]
        then
            let symbol = grid.[posCandidateWithLevel0]
            if posCandidate.L = 0 && symbol = "ZZ"   // allow ZZ on L 0
            then true
            else if posCandidate.L = 0
            then  // block other outer portals on L 0
                let portal = portalMap.[symbol]
                let currentPosWithLevel0 = { currentPos with L = 0 }
                let portalDir = if currentPosWithLevel0 = portal.Apos then portal.Adir else portal.Bdir
                portalDir = Down
            else not (posCandidate.L > 0 && symbol = "ZZ")   // block ZZ on L > 0
        else conditions1

    let getMinDistancePart2 (grid: Map<Position, string>) (portalMap: Map<string, Portal>) source target =
        let startPath = { Pos = source; Symbol = grid.[source]; Distance = 0; Track = [source] }
        let queue = System.Collections.Generic.Queue<Path>()
        let mutable foundPath = None

        queue.Enqueue startPath
        while (queue.Count <> 0 && Option.isNone foundPath) do
            let currentPath = queue.Peek()
            queue.Dequeue() |> ignore

            let adjacentPositions = getAdjacentPositions currentPath.Pos
                                    |> List.filter (isLegalNextPositionPart2 grid portalMap currentPath.Track currentPath.Pos)
                                    |> List.filter (fun p -> p.L < 50)
            for adjacentPos in adjacentPositions do
                let adjacentPosWithLevel0 = { adjacentPos with L = 0 }
                let symbol = grid.[adjacentPosWithLevel0]
                let newDistance = currentPath.Distance + 1
                if adjacentPos = target
                then
                    let newTrack = currentPath.Track @ [adjacentPos]
                    foundPath <- Some { Pos = adjacentPos; Symbol = symbol; Distance = newDistance; Track = newTrack }
                else
                    let newPath = if symbol = "."
                                  then
                                    let newTrack = currentPath.Track @ [adjacentPos]
                                    { Pos = adjacentPos; Symbol = symbol; Distance = newDistance; Track = newTrack }
                                  else if isPortal symbol
                                  then
                                     let portal = portalMap.[symbol]
                                     let currentPosAsLevel0 = { currentPath.Pos with L = 0 }
                                     let otherSide = if portal.Apos = currentPosAsLevel0 then portal.Bpos else portal.Apos

                                     let direction = if portal.Apos = currentPosAsLevel0 then portal.Adir else portal.Bdir
                                     let otherSideLevel = if direction = Down then currentPath.Pos.L + 1 else currentPath.Pos.L - 1

                                     let otherSide' = { otherSide with L = otherSideLevel }
                                     let adjacentPortalPos = getAdjacentPortalPos grid otherSide
                                     let adjacentPortalPos' = { adjacentPortalPos with L = otherSideLevel }
                                     let newTrack = currentPath.Track @ [adjacentPos; adjacentPortalPos'; otherSide']
                                     printfn "passing portal %s in direction %A into level %d" portal.Name direction otherSideLevel
                                     { Pos = otherSide'; Symbol = symbol; Distance = newDistance; Track = newTrack }
                                  else
                                    printfn "symbol |%s| %A" symbol adjacentPos
                                    System.Exception("bad") |> raise
                    queue.Enqueue newPath
        foundPath.Value

    let day20Part2 () =
        let (grid, portalMap, aaStart, zzStart) = getGridAndPortals InputFile
        let d = getMinDistancePart2 grid portalMap aaStart zzStart
        d.Distance
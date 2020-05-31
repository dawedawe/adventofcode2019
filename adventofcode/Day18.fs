namespace Adventofcode

module Day18 =

    [<Literal>]
    let InputFile = "Day18Input.txt"

    type Position = { X: int; Y: int }

    type Location = {
        Pos : Position
        Symbol : char
        Distance : int
    }

    let getGrid path =
        let lines = System.IO.File.ReadAllLines path
        let mutable grid = Map.empty
        for y in [ 0 .. lines.Length - 1 ] do
            let chars = lines.[y].ToCharArray()
            for x in [ 0 .. chars.Length - 1 ] do
                let pos = { X = x; Y = y }
                grid <- grid.Add (pos, chars.[x])
        grid

    let printGrid (grid : Map<Position, char>) =
        let nodes = Map.toList grid
        let maxY = nodes |> List.maxBy (fun (p, _) -> p.Y) |> fun (p, _) -> p.Y
        for y in [0 .. maxY] do
            let lineNodes = nodes |> List.filter (fun (p, _) -> p.Y = y)
                                   |> List.sortBy (fun (p, _) -> p.X)
            let mutable lineString = ""
            for node in lineNodes do
                lineString <- lineString + string(snd node)
            printfn "%s" lineString

    let getAdjacentPositions (pos : Position) =
        let northPos = { pos with Y = pos.Y + 1 }
        let southPos = { pos with Y = pos.Y - 1 }
        let westPos  = { pos with X = pos.X - 1 }
        let eastPos  = { pos with X = pos.X + 1 }
        [ northPos; southPos; westPos; eastPos; ]

    let isDoor c = ['A' .. 'Z'] |> List.contains c

    let isKey c = ['a' .. 'z'] |> List.contains c

    type Path = {
        Pos: Position
        Symbol: char
        NeededKeys: char list
        Distance: int
    }

    let getMinDistanceAndNeededKeys grid key otherKey =
        let startPos = grid |> Map.filter (fun k v -> v = key) |> Map.toArray |> Array.item 0 |> fst
        let startPath = { Pos = startPos; Symbol = key; Distance = 0; NeededKeys = List.empty }
        let queue = System.Collections.Generic.Queue<Path>()
        let mutable visited = Set.singleton startPos
        let mutable foundPath = Option.None

        queue.Enqueue startPath
        while (queue.Count <> 0 && Option.isNone foundPath) do
            let currentPath = queue.Peek()
            queue.Dequeue() |> ignore

            let adjacentPositions = getAdjacentPositions currentPath.Pos
            for adjacentPos in adjacentPositions do
                if grid.ContainsKey adjacentPos && grid.[adjacentPos] <> '#' && not (visited.Contains adjacentPos)
                then
                    let symbol = grid.[adjacentPos]
                    let neededKeys = if isDoor symbol then currentPath.NeededKeys @ [System.Char.ToLower symbol] else currentPath.NeededKeys
                    let newPath = { Pos = adjacentPos; Symbol = symbol; Distance = currentPath.Distance + 1; NeededKeys = neededKeys }
                    if symbol = otherKey
                    then foundPath <- Some newPath
                    else
                        visited <- visited.Add adjacentPos
                        queue.Enqueue newPath
        (foundPath.Value.Distance, foundPath.Value.NeededKeys)

    type Edge = { A: char; B: char }
    let makeEdge a b = { A = a; B = b; }

    let getDistances (grid : Map<Position, char>) =
        let mutable matrix = Map.empty
        let keys = grid |> Map.filter (fun _ v -> isKey v) |> Map.toList |> List.map (fun (_, v) -> v) |> (@) ['@']
        for key in keys do
            let otherKeys = keys |> List.filter (fun k -> k <> key)
            for otherKey in otherKeys do
                if not (matrix.ContainsKey (makeEdge otherKey key))
                then
                    let (minDistance, neededKeys) = getMinDistanceAndNeededKeys grid key otherKey
                    let entry = (makeEdge key otherKey, (minDistance, neededKeys))
                    printfn "key %c -> %c: %d (%A)" key otherKey minDistance neededKeys
                    matrix <- matrix.Add entry
        matrix

    type PathAndDistance = { Path: List<char>; Dist: int }

    let rec collectKeys matrix (allKeys: Set<char>) currentKey (remainingKeys: Set<char>) (cache: System.Collections.Generic.Dictionary<char * Set<char>, int>) =
        if (Set.isEmpty remainingKeys)
        then 0
        else
            let cacheKey = (currentKey, remainingKeys)
            if cache.ContainsKey cacheKey
            then
                cache.[cacheKey]
            else

                let mutable result = System.Int32.MaxValue
                let collectedKeys = allKeys - remainingKeys
                let edgesOfCurrentPos = matrix |> Map.filter (fun edge v -> (edge.A = currentKey || edge.B = currentKey) &&
                                                                            (snd v |> List.forall (collectedKeys.Contains)))
                let reachableKeys = edgesOfCurrentPos |> Map.toList |> List.filter (fun (edge, _) -> if edge.A = currentKey then not (collectedKeys.Contains edge.B)
                                                                                                                            else not (collectedKeys.Contains edge.A))
                                    
                for (edge, (dist, _)) in reachableKeys do
                    let key = if edge.A = currentKey then edge.B else edge.A
                    let remainingKeys' = remainingKeys.Remove key
                    let d = dist + collectKeys matrix allKeys key remainingKeys' cache
                    result <- min result d
                
                cache.Add (cacheKey, result)
                result

    let day18 () =
        let grid = getGrid InputFile
        printGrid grid
        let matrix = getDistances grid
        let remainingKeys = matrix |> Map.toList |> List.collect (fun (e, _) -> [e.A; e.B]) |> Set.ofList |> Set.remove '@'
        let allKeys = matrix |> Map.toList |> List.collect (fun (e, _) -> [e.A; e.B]) |> Set.ofList |> Set.remove '@'
        let cache = System.Collections.Generic.Dictionary<char * Set<char>, int>()
        let r = collectKeys matrix allKeys '@' remainingKeys cache
        r

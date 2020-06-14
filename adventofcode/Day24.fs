namespace Adventofcode

module Day24 =

    [<Literal>]
    let InputFile = "Day24Input.txt"

    let getGrid path =
        System.IO.File.ReadAllLines path
        |> Array.map (fun s -> s.ToCharArray())

    let countBugsInAdjacent (grid: char [] []) y x =
        let adjacentPositions =
            [ (y - 1, x)
              (y + 1, x)
              (y, x - 1)
              (y, x + 1) ]
            |> List.filter (fun (y, x) ->
                y >= 0
                && y < grid.Length
                && x >= 0
                && x < grid.[0].Length)

        adjacentPositions
        |> List.sumBy (fun (y, x) -> if grid.[y].[x] = '#' then 1 else 0)

    let calcNextGen (grid: char [] []) y x =
        let bugsInAdjacent = countBugsInAdjacent grid y x
        let symbol = grid.[y].[x]
        match (symbol, bugsInAdjacent) with
        | ('#', 1) -> '#'
        | ('#', _) -> '.'
        | ('.', x) when x = 1 || x = 2 -> '#'
        | ('.', _) -> '.'
        | _ -> failwith "bad state"

    let calcMinute (grid: char [] []) =
        let grid' = Array.map Array.copy grid
        for y in [ 0 .. grid.Length - 1 ] do
            for x in [ 0 .. grid.[y].Length - 1 ] do
                let c = calcNextGen grid y x
                grid'.[y].[x] <- c
        grid'

    let rec calcTillRepeat (grids: Set<char [] []>) (grid: char [] []) =
        let grid' = calcMinute grid
        if (Set.contains grid' grids) then
            grid'
        else
            let grids' = Set.add grid' grids
            calcTillRepeat grids' grid'

    let calcBioDiversity (grid: char [] []) =
        let lastExponent = grid.Length * grid.[0].Length - 1
        grid
        |> Array.collect id
        |> Array.zip [| 0 .. lastExponent |]
        |> Array.sumBy (fun (e, c) -> if (c = '#') then System.Math.Pow(2.0, float e) else 0.0)
        |> int

    let day24 () =
        let grid = getGrid InputFile
        let repeatedGrid = calcTillRepeat Set.empty grid
        calcBioDiversity repeatedGrid

    type Grid = { Field: char [] []; Level: int }

    let createEmptyGrid level =
        let field =
            Array.map (fun _ -> Array.create 5 '.') [| 1 .. 5 |]

        field.[2].[2] <- '?'
        { Field = field; Level = level }

    let addLevels (grids: Map<int, Grid>) =
        let gridsList = Map.toList grids |> List.sortBy fst
        let currentMin = gridsList |> List.head |> fst
        let currentMax = gridsList |> List.last |> fst
        let newMin = createEmptyGrid (currentMin - 1)
        let newMax = createEmptyGrid (currentMax + 1)
        grids
        |> Map.add newMin.Level newMin
        |> Map.add newMax.Level newMax

    let copyGrid (grid: Grid) =
        let fieldCopy = Array.map Array.copy grid.Field
        { Field = fieldCopy
          Level = grid.Level }

    let copyGrids (grids: Map<int, Grid>) =
        let gridsList = Map.toList grids

        let gridsListCopy =
            gridsList
            |> List.map (fun (k, v) -> (k, copyGrid v))

        gridsListCopy |> Map.ofList

    let getAdjacentTiles (grids: Map<int, Grid>) (grid: Grid) y x =
        let innerGrid =
            if grids.ContainsKey(grid.Level + 1) then
                grids.[grid.Level + 1].Field
            else
                (createEmptyGrid (grid.Level + 1)).Field

        let outerGrid =
            if grids.ContainsKey(grid.Level - 1) then
                grids.[grid.Level - 1].Field
            else
                (createEmptyGrid (grid.Level - 1)).Field

        match (y, x) with
        | (0, 0) ->
            [| outerGrid.[1].[2]
               outerGrid.[2].[1]
               grid.Field.[0].[1]
               grid.Field.[1].[0] |]
        | (0, 1) ->
            [| outerGrid.[1].[2]
               grid.Field.[0].[0]
               grid.Field.[0].[2]
               grid.Field.[1].[1] |]
        | (0, 2) ->
            [| outerGrid.[1].[2]
               grid.Field.[0].[1]
               grid.Field.[0].[3]
               grid.Field.[1].[2] |]
        | (0, 3) ->
            [| outerGrid.[1].[2]
               grid.Field.[0].[2]
               grid.Field.[0].[4]
               grid.Field.[1].[3] |]
        | (0, 4) ->
            [| outerGrid.[1].[2]
               outerGrid.[2].[3]
               grid.Field.[0].[3]
               grid.Field.[1].[4] |]

        | (1, 0) ->
            [| outerGrid.[2].[1]
               grid.Field.[0].[0]
               grid.Field.[1].[1]
               grid.Field.[2].[0] |]
        | (1, 1) ->
            [| grid.Field.[1].[0]
               grid.Field.[0].[1]
               grid.Field.[1].[2]
               grid.Field.[2].[1] |]
        | (1, 2) ->
            [| grid.Field.[1].[1]
               grid.Field.[0].[2]
               grid.Field.[1].[3]
               innerGrid.[0].[0]
               innerGrid.[0].[1]
               innerGrid.[0].[2]
               innerGrid.[0].[3]
               innerGrid.[0].[4] |]
        | (1, 3) ->
            [| grid.Field.[1].[2]
               grid.Field.[0].[3]
               grid.Field.[1].[4]
               grid.Field.[2].[3] |]
        | (1, 4) ->
            [| grid.Field.[1].[3]
               grid.Field.[0].[4]
               grid.Field.[2].[4]
               outerGrid.[2].[3] |]

        | (2, 0) ->
            [| outerGrid.[2].[1]
               grid.Field.[1].[0]
               grid.Field.[2].[1]
               grid.Field.[3].[0] |]
        | (2, 1) ->
            [| grid.Field.[2].[0]
               grid.Field.[1].[1]
               grid.Field.[3].[1]
               innerGrid.[0].[0]
               innerGrid.[1].[0]
               innerGrid.[2].[0]
               innerGrid.[3].[0]
               innerGrid.[4].[0] |]
        | (2, 2) ->
            [| grid.Field.[2].[1]
               grid.Field.[1].[2]
               grid.Field.[2].[3]
               grid.Field.[3].[2] |]
        | (2, 3) ->
            [| grid.Field.[1].[3]
               grid.Field.[2].[4]
               grid.Field.[3].[3]
               innerGrid.[0].[4]
               innerGrid.[1].[4]
               innerGrid.[2].[4]
               innerGrid.[3].[4]
               innerGrid.[4].[4] |]
        | (2, 4) ->
            [| grid.Field.[2].[3]
               grid.Field.[1].[4]
               grid.Field.[3].[4]
               outerGrid.[2].[3] |]

        | (3, 0) ->
            [| outerGrid.[2].[1]
               grid.Field.[2].[0]
               grid.Field.[3].[1]
               grid.Field.[4].[0] |]
        | (3, 1) ->
            [| grid.Field.[3].[0]
               grid.Field.[2].[1]
               grid.Field.[3].[2]
               grid.Field.[4].[1] |]
        | (3, 2) ->
            [| grid.Field.[3].[1]
               grid.Field.[3].[3]
               grid.Field.[4].[2]
               innerGrid.[4].[0]
               innerGrid.[4].[1]
               innerGrid.[4].[2]
               innerGrid.[4].[3]
               innerGrid.[4].[4] |]
        | (3, 3) ->
            [| grid.Field.[3].[2]
               grid.Field.[2].[3]
               grid.Field.[3].[4]
               grid.Field.[4].[3] |]
        | (3, 4) ->
            [| grid.Field.[3].[3]
               grid.Field.[2].[4]
               grid.Field.[4].[4]
               outerGrid.[2].[3] |]

        | (4, 0) ->
            [| outerGrid.[2].[1]
               outerGrid.[3].[2]
               grid.Field.[3].[0]
               grid.Field.[4].[1] |]
        | (4, 1) ->
            [| outerGrid.[3].[2]
               grid.Field.[4].[0]
               grid.Field.[3].[1]
               grid.Field.[4].[2] |]
        | (4, 2) ->
            [| outerGrid.[3].[2]
               grid.Field.[4].[1]
               grid.Field.[3].[2]
               grid.Field.[4].[3] |]
        | (4, 3) ->
            [| outerGrid.[3].[2]
               grid.Field.[4].[2]
               grid.Field.[3].[3]
               grid.Field.[4].[4] |]
        | (4, 4) ->
            [| outerGrid.[3].[2]
               outerGrid.[2].[3]
               grid.Field.[4].[3]
               grid.Field.[3].[4] |]

        | _ -> failwith "bad"

    let countBugsInAdjacentPart2 grids grid y x =
        let adjacentPositions = getAdjacentTiles grids grid y x
        adjacentPositions
        |> Array.sumBy (fun c -> if c = '#' then 1 else 0)

    let calcNextGenPart2 grids grid y x =
        let bugsInAdjacent = countBugsInAdjacentPart2 grids grid y x
        let symbol = grid.Field.[y].[x]
        match (symbol, bugsInAdjacent) with
        | ('#', 1) -> '#'
        | ('#', _) -> '.'
        | ('.', x) when x = 1 || x = 2 -> '#'
        | ('.', _) -> '.'
        | ('?', _) -> '?'
        | _ -> failwith "bad state"

    let calcField (grids: Map<int, Grid>) grid =
        let grid' = copyGrid grid
        for y in [ 0 .. grid.Field.Length - 1 ] do
            for x in [ 0 .. grid.Field.[y].Length - 1 ] do
                let c = calcNextGenPart2 grids grid y x
                grid'.Field.[y].[x] <- c
        grid'

    let calcMinutePart2 (grids: Map<int, Grid>) =
        let currentGrids = addLevels grids
        currentGrids
        |> Map.map (fun k v -> calcField currentGrids v)

    let rec calcNMinutes n grids =
        if n = 0 then
            grids
        else
            let n' = n - 1
            let grid' = calcMinutePart2 grids
            calcNMinutes n' grid'

    let countSingleGrid (grid: Grid) =
        grid.Field
        |> Array.collect id
        |> Array.sumBy (fun c -> if c = '#' then 1 else 0)

    let countBugs (grids: Map<int, Grid>) =
        let gridList = grids |> Map.toArray
        gridList
        |> Array.sumBy (fun (k, v) -> countSingleGrid v)

    let day24Part2 () =
        let field0 = getGrid InputFile
        field0.[2].[2] <- '?'
        let grid0 = { Field = field0; Level = 0 }
        let grids = Map.empty.Add(grid0.Level, grid0)
        let grids' = calcNMinutes 200 grids
        let count = countBugs grids'
        count

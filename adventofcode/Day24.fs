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

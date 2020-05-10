namespace Adventofcode

module Day10 =

    [<Literal>]
    let InputFile = "Day10Input.txt"

    type Target = {
        Angle : float
        Distance : float
        Xpos : int
        Ypos : int
        mutable Vaporized : bool
    }

    let getMap path =
        System.IO.File.ReadAllLines path
        |> Array.map (fun s -> s.ToCharArray())

    let calcAngle (xDist : double) (yDist : double) =
        let tanAlpha = abs yDist / abs xDist
        let atan = System.Math.Atan tanAlpha
        let d = atan * (180.0 / System.Math.PI)
        match (xDist, yDist) with
        | x, y when x >= 0.0 && y < 0.0 -> 90.0 - d    // 1 q
        | x, y when x <= 0.0 && y <= 0.0 -> 270.0 + d   // 2 q
        | x, y when x < 0.0 && y > 0.0 -> 270.0 - d     // 3 q
        | x, y when x >= 0.0 && y >= 0.0 -> 90.0 + d   // 4 q
        | _ -> System.ArgumentException("") |> raise

    let getAsteroidPositions (map : char [] []) =
        let mutable positions  = List.empty<int * int>
        let linesCount = Array.length map
        let coloumnsCount = Array.length map.[0]
        for y in [0 .. linesCount - 1] do
            for x in [0 .. coloumnsCount - 1] do
                if map.[y].[x] = '#'
                then positions <- List.append positions [(y, x)]
        positions

    let calcDist ((x1, y1) : int * int) ((x2, y2) : int * int) =
        let a = x1 - x2
        let b = y1 - y2
        let cSquared = (a * a + b * b)
        let c = System.Math.Sqrt (float cSquared)
        c

    let calcUniqueAnglesToOthers (candidate : int * int) (asteroids : (int * int) list) =
        let mutable uniqueAngles = Set.empty<float>
        let others = List.filter (fun c -> c <> candidate) asteroids
        let (yCandidate, xCandidate) = fst candidate, snd candidate
        for (y, x) in others do
            let xDist, yDist = (xCandidate - x, yCandidate - y)
            let angle = calcAngle (double xDist) (double yDist)
            uniqueAngles <- uniqueAngles.Add angle
        Set.count uniqueAngles

    let calcAnglesAndTargets (candidate : int * int) (asteroids : (int * int) list) =
        let mutable anglesAndTargets = List.empty<Target>
        let others = List.filter (fun c -> c <> candidate) asteroids
        let (yCandidate, xCandidate) = fst candidate, snd candidate
        for (y, x) in others do
            let xDist, yDist = (x - xCandidate, y - yCandidate)
            let angle = calcAngle (double xDist) (double yDist)
            let distance = calcDist (xCandidate, yCandidate) (x, y)
            let target = { Angle = angle; Distance =  distance; Xpos = x; Ypos = y; Vaporized = false }
            anglesAndTargets <- target :: anglesAndTargets
        anglesAndTargets
        |> List.sortBy (fun t -> t.Angle)

    let findBestPosition (positions : ((int * int) list)) =
        let mutable scores = List.empty<(int * int) *int>
        for candidate in positions do
            let angles = calcUniqueAnglesToOthers candidate positions
            scores <- List.append scores [(candidate, angles)]
        List.maxBy (fun (p, s) -> s) scores

    let day10 () =
        let map = getMap InputFile
        let positions = getAsteroidPositions map
        let best = findBestPosition positions
        best

    let doRound alreadyShot (aAndDByAngle : list<float * list<Target>>) =
        let angles = List.map (fun (k, _) -> k) aAndDByAngle
        let mutable vaporized = alreadyShot
        let mutable target200 = Option.None
        for angle in angles do
            let targetsAtAngle = List.filter (fun (k, _) -> k = angle) aAndDByAngle
                                 |> List.head
                                 |> snd
            let target = List.tryFind (fun t -> t.Vaporized = false) targetsAtAngle
            if (Option.isSome target)
            then
                target.Value.Vaporized <- true
                vaporized <- vaporized + 1
                if (vaporized = 200)
                then target200 <- Some target.Value

        (target200, vaporized)

    let day10Part2 () =
        let map = getMap InputFile
        let positions = getAsteroidPositions map
        let (best, count) = findBestPosition positions
        let aAndD = calcAnglesAndTargets best positions
        let aAndDByAngle = aAndD
                           |> List.groupBy (fun t -> t.Angle)
                           |> List.map (fun (k, g) -> (k, List.sortBy (fun t -> t.Distance) g))

        let mutable target200 = Option.None
        let mutable alreadyShot = 0        
        while (Option.isNone target200) do
             let (target200', alreadyShot') = doRound alreadyShot aAndDByAngle
             target200 <- target200'
             alreadyShot <- alreadyShot'
        printfn "%A" target200
        target200.Value.Xpos * 100 + target200.Value.Ypos

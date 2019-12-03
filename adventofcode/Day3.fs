namespace Adventofcode

module Day3 =

    open System.Linq

    [<Literal>]
    let InputFile = "Day3Input.txt"

    type Direction =
    | Up
    | Down
    | Left
    | Right

    type PathInstruction = { Dir : Direction; Steps : int }

    let parsePathInstruction (s : string) =
        let dir = s.[0]
        let steps = s.Substring(1) |> System.Int32.Parse
        match dir with
        | 'U' -> { Dir = Up; Steps = steps }
        | 'D' -> { Dir = Down; Steps = steps }
        | 'L' -> { Dir = Left; Steps = steps }
        | 'R' -> { Dir = Right; Steps = steps }
        | _   -> System.ArgumentException(s) |> raise

    let getWirePaths path =
        let lines = System.IO.File.ReadAllLines path
        let paths = Array.map (fun (l : string) -> l.Split [|','|]) lines
        let path1 = Array.map parsePathInstruction paths.[0]
        let path2 = Array.map parsePathInstruction paths.[1]
        (path1, path2)

    let move (positions : System.Collections.Generic.List<int * int>) (pathInstruction : PathInstruction) =
        let f = match pathInstruction.Dir with
                | Up -> (fun (x, y)-> (x, y + 1))
                | Down -> (fun (x, y)-> (x, y - 1))
                | Left -> (fun (x, y)-> (x - 1, y))
                | Right -> (fun (x, y)-> (x + 1, y))
        for _ in 1 .. pathInstruction.Steps do
            let pos = positions.Last() |> f
            positions.Add pos

    let followPath (path : PathInstruction []) =
        let positions = System.Collections.Generic.List<int * int>()
        positions.Add((0, 0))
        for p in path do
            move positions p
        positions
    
    let manhattanDistance (x, y) =
        abs x + abs y

    let crosses (positions1 : System.Collections.Generic.List<int * int>) (positions2 : System.Collections.Generic.List<int * int>) =
        let s1 = Set.ofSeq positions1
        let s2 = Set.ofSeq positions2
        s1.Intersect s2
        |> List.ofSeq
        |> List.filter (fun p -> p <> (0, 0))
        |> List.minBy manhattanDistance
        |> manhattanDistance

    let day3 () =
        let (p1, p2) = getWirePaths InputFile
        let positions1 = followPath p1
        let positions2 = followPath p2
        let c = crosses positions1 positions2
        c
        
namespace Adventofcode

module Day15 =

    [<Literal>]
    let InputFile = "Day15Input.txt"

    type ParameterMode =
    | Position  // 0
    | Immediate // 1
    | Relative  // 2

    type Opcodes =
    | Plus
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | RelativeBaseOffset
    | Halt

    type Program = {
        mutable Memory : int64 []
        mutable RelativeBase : int
        mutable Inputs : int64 []
    }

    let parseInstruction inst =
        let opcodeCode = inst % 100
        let opcode, parameterCount = match opcodeCode with
                                     | 1 -> (Plus, 3)
                                     | 2 -> (Multiply, 3)
                                     | 3 -> (Input, 1)
                                     | 4 -> (Output, 1)
                                     | 5 -> (JumpIfTrue, 2)
                                     | 6 -> (JumpIfFalse, 2)
                                     | 7 -> (LessThan, 3)
                                     | 8 -> (Equals, 3)
                                     | 9 -> (RelativeBaseOffset, 1)
                                     | 99 -> (Halt, 0)
                                     | _ -> System.ArgumentException("unknown opcodeCode " + string opcodeCode) |> raise
        let mutable parameterModes = Array.empty
        let mutable modeNumbers = inst / 100
        for _ in 1 .. parameterCount do
            if modeNumbers > 0
            then
                let mode = match (modeNumbers % 10) with
                           | 0 -> Position
                           | 1 -> Immediate
                           | 2 -> Relative
                           | _ -> System.ArgumentException("unknown modeNumber") |> raise
                parameterModes <- Array.append parameterModes [|mode|]
                modeNumbers <- modeNumbers / 10
            else
                parameterModes <- Array.append parameterModes [|Position|]
        (opcode, parameterModes)

    let getProgram path =
        let input = System.IO.File.ReadAllLines path
        let memory = input.[0].Split [|','|]
                      |> Array.map System.Int64.Parse
        { Memory = memory; RelativeBase = 0; Inputs = [| |] }
    
    let resizeIfNeeded (program : Program) memoryPosition =
        if memoryPosition >= program.Memory.Length
        then let diff = memoryPosition - program.Memory.Length + 1
             let bonus = Array.create diff 0L
             let memory = Array.append program.Memory bonus
             program.Memory <- memory

    let getParameter mode (parameter : int64) (program : Program) =
        match mode with
        | Immediate -> parameter
        | Position -> resizeIfNeeded program (int parameter)
                      program.Memory.[int parameter]
        | Relative -> let memPos = program.RelativeBase + (int parameter)
                      resizeIfNeeded program memPos
                      program.Memory.[memPos]

    let getMemPos mode parameter (program : Program) =
        match mode with
        | Position -> parameter |> int
        | Relative -> let memPos = program.RelativeBase + parameter
                      memPos |> int
        | _        -> System.ArgumentException("memory position argument should be in position or relative mode") |> raise

    let doOpcode (opcode : Opcodes) (parameterModes : ParameterMode []) (parameters : int64 []) (program : Program) =
        match opcode with
        | Input                    -> let memPos = getMemPos parameterModes.[0] (int parameters.[0]) program
                                      resizeIfNeeded program memPos
                                      program.Memory.[memPos] <- Array.head program.Inputs
                                      program.Inputs <- program.Inputs.[1..]
                                      (None, None)
        | Output                   -> let p = getParameter parameterModes.[0] parameters.[0] program
                                      // printfn "Output = %d" p
                                      (None, Some p)
        | Plus | Multiply          -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                                      let p2 = getParameter parameterModes.[1] parameters.[1] program
                                      let memPos = getMemPos parameterModes.[2] (int parameters.[2]) program
                                      resizeIfNeeded program memPos
                                      let op = if opcode = Plus then (+) else (*)
                                      program.Memory.[memPos] <- op p1 p2
                                      (None, None)
        | JumpIfTrue | JumpIfFalse -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                                      let p2 = getParameter parameterModes.[1] parameters.[1] program
                                      let op = if opcode = JumpIfTrue then (<>) else (=)
                                      if op p1 0L then (Some p2, None) else (None, None)
        | LessThan | Equals        -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                                      let p2 = getParameter parameterModes.[1] parameters.[1] program
                                      let memPos = getMemPos parameterModes.[2] (int parameters.[2]) program
                                      resizeIfNeeded program memPos
                                      let op = if opcode = LessThan then (<) else (=)
                                      program.Memory.[memPos] <- if op p1 p2 then 1L else 0L
                                      (None, None)
        | RelativeBaseOffset       -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                                      program.RelativeBase <- program.RelativeBase + int p1
                                      (None, None)
        | _                        -> System.ArgumentException("doOpcode(): unsupported opcode") |> raise

    type Direction =
    | North = 1
    | South = 2
    | West = 3
    | East = 4

    type StatusCode =
    | HitWall = 0   // The repair droid hit a wall. Its position has not changed.
    | Moved   = 1   // The repair droid has moved one step in the requested direction.
    | Oxygen  = 2   // The repair droid has moved one step in the requested direction; its new position is the location of the oxygen system.

    type Position = {
        X : int
        Y : int
    }

    type State = {
        Grid       : Map<Position, char>
        CurrentPos : Position
        OxygenPos  : Option<Position>
    }

    let printGrid (grid : Map<Position, char>) =
        let panels = Map.toList grid
        let firstLineNumber = panels |> List.maxBy (fun (p, _) -> p.Y) |> fun (p, _) -> p.Y
        let lastLineNumber = panels |> List.minBy (fun (p, _) -> p.Y) |> fun (p, _) -> p.Y
        let firstColumnNumber = panels |> List.minBy (fun (p, _) -> p.X) |> fun (p, _) -> p.X
        let lastColumnNumber = panels |> List.maxBy (fun (p, _) -> p.X) |> fun (p, _) -> p.X
        for lineNumber in [firstLineNumber .. -1 .. lastLineNumber] do
            let linePanels = panels |> List.filter (fun (p, _) -> p.Y = lineNumber)
                                    |> List.sortBy (fun (p, _) -> p.X)
            let mutable lineString = ""
            for column in [firstColumnNumber .. lastColumnNumber] do
                let panelInColumn = linePanels |> List.tryFind (fun (p, _) -> p.X = column)
                if Option.isSome panelInColumn
                then lineString <- lineString + ((snd panelInColumn.Value) |> string)
                else lineString <- lineString + " "
            printfn "%s" lineString

    let runProgram (program : Program) =
        let mutable halt = false
        let mutable opcodePos = 0
        let mutable output = None
        let mutable lastOutput = -1L
        let mutable allInputsUsed = false

        while not halt && not allInputsUsed do
            let (opcode, parameterModes) = parseInstruction (int program.Memory.[opcodePos])
            if opcode = Halt
            then halt <- true
            else
                let parameters = Array.skip(opcodePos + 1) program.Memory |> Array.take parameterModes.Length
                System.Diagnostics.Debug.Assert(parameters.Length = parameterModes.Length)
                
                if opcode = Opcodes.Input && Array.isEmpty program.Inputs
                then allInputsUsed <- true
                else
                    let ipModified, outputMade = doOpcode opcode parameterModes parameters program
                    output <- outputMade
                    match ipModified with
                    | Some p -> opcodePos <- int p
                    | None   -> opcodePos <- opcodePos + 1 + parameterModes.Length
                    resizeIfNeeded program opcodePos

                if Option.isSome output
                then lastOutput <- output.Value
        
        lastOutput

    type Location = {
        Pos : Position
        Distance : int
        Path : Direction []
    }

    let getAdjacentPositionsAndDirections (pos : Position) =
        let northPos = { pos with Y = pos.Y + 1 }
        let southPos = { pos with Y = pos.Y - 1 }
        let westPos  = { pos with X = pos.X - 1 }
        let eastPos  = { pos with X = pos.X + 1 }
        [ (northPos, Direction.North); (southPos, Direction.South); (westPos, Direction.West); (eastPos, Direction.East); ]

    let updateGrid (grid : Map<Position, char>) posToUpdate (output : StatusCode) =
        match output with
        | StatusCode.HitWall -> grid.Add (posToUpdate, '#')
        | StatusCode.Moved   -> grid.Add (posToUpdate, '.')
        | StatusCode.Oxygen  -> grid.Add (posToUpdate, 'O')
        | _                  -> System.Exception("bad output") |> raise

    let copyProgram (program : Program) =
        let inputs = Array.copy program.Inputs
        let memory = Array.copy program.Memory
        let relativeBase = program.RelativeBase
        let copy = { Inputs = inputs; Memory = memory; RelativeBase = relativeBase }
        copy

    let bfs (program : Program) =
        let initPos = { X = 0; Y = 0; }
        let initLocation = { Pos = initPos; Distance = 0; Path = Array.empty }
        let mutable grid = Map.empty.Add (initPos, '.')
        let queue = System.Collections.Generic.Queue<Location>()
        let mutable oxygenLocation = None
        queue.Enqueue initLocation
        
        while (queue.Count <> 0 && Option.isNone oxygenLocation) do
            let currentLocation = queue.Peek()
            let inputsToCurrentLocation = currentLocation.Path |> Array.map int64
            if (inputsToCurrentLocation.Length > 0)
            then
                let program' = { (copyProgram program) with Inputs = inputsToCurrentLocation }
                let output = runProgram program'
                if output = (int64 StatusCode.Oxygen)
                then
                    printfn "Oxygen reached in %d steps" currentLocation.Distance
                    oxygenLocation <- Some currentLocation
                
            queue.Dequeue() |> ignore

            let adjacentPositions = getAdjacentPositionsAndDirections currentLocation.Pos
            for adjacentPos in adjacentPositions do
                if not (grid.ContainsKey (fst adjacentPos))
                then
                    let pathToAdjacent = Array.append currentLocation.Path [| snd adjacentPos |]
                    let adjacentLocation = { Pos = fst adjacentPos; Distance = currentLocation.Distance + 1; Path = pathToAdjacent }
                    let inputsToAdjacentLocation = adjacentLocation.Path |> Array.map int64
                    let program'' = { (copyProgram program) with Inputs = inputsToAdjacentLocation }
                    let output'' = runProgram program'' |> int |> enum<StatusCode>
                    grid <- updateGrid grid adjacentLocation.Pos output''
                    printGrid grid
                    if (output'' <> StatusCode.HitWall)
                    then
                        queue.Enqueue adjacentLocation

        oxygenLocation.Value

    let day15 () =
        let program = getProgram InputFile
        let d = bfs program
        d

    let bfsForWholeGrid (program : Program) =
        let initPos = { X = 0; Y = 0; }
        let initLocation = { Pos = initPos; Distance = 0; Path = Array.empty }
        let mutable grid = Map.empty.Add (initPos, '.')
        let queue = System.Collections.Generic.Queue<Location>()
        queue.Enqueue initLocation
        
        while (queue.Count <> 0) do
            let currentLocation = queue.Peek()
            queue.Dequeue() |> ignore

            let adjacentPositionsAndDirections = getAdjacentPositionsAndDirections currentLocation.Pos
            for (adjacentPos, adjacentDir) in adjacentPositionsAndDirections do
                if not (grid.ContainsKey adjacentPos)
                then
                    let pathToAdjacent = Array.append currentLocation.Path [| adjacentDir |]
                    let adjacentLocation = { Pos = adjacentPos; Distance = currentLocation.Distance + 1; Path = pathToAdjacent }
                    let inputsToAdjacentLocation = adjacentLocation.Path |> Array.map int64
                    let program' = { (copyProgram program) with Inputs = inputsToAdjacentLocation }
                    let output = runProgram program' |> int |> enum<StatusCode>
                    grid <- updateGrid grid adjacentLocation.Pos output
                    if (output <> StatusCode.HitWall)
                    then queue.Enqueue adjacentLocation

        grid

    let rec fillGridWithOxygen (grid : Map<Position, char>) (minutesSoFar : int) =
        let isGridFilled = grid |> Map.filter (fun _ v -> v = '.') |> Map.count = 0
        if isGridFilled
        then
            minutesSoFar
        else
            let mutable grid' = grid
            let oxygenLocations = grid' |> Map.filter (fun _ v -> v = 'O') |> Map.toList |> List.map fst
            for oxygenLocation in oxygenLocations do
                let adjacentPositions = getAdjacentPositionsAndDirections oxygenLocation
                                        |> List.map fst
                                        |> List.filter (fun p -> grid.[p] = '.')
                for adjacentPosition in adjacentPositions do
                    grid' <- grid'.Add (adjacentPosition, 'O')
            let minutesSoFar' = minutesSoFar + 1
            fillGridWithOxygen grid' minutesSoFar'

    let day15Part2 () =
        let grid = getProgram InputFile
                   |> bfsForWholeGrid
        let minutesNeeded = fillGridWithOxygen grid 0
        minutesNeeded
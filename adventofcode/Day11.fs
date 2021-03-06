namespace Adventofcode

module Day11 =

    [<Literal>]
    let InputFile = "Day11Input.txt"

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

    type Color =
    | Black = 0
    | White = 1

    type Panel = {
        mutable Color : Color
        mutable TimesPainted : int
    }

    type Turn =
    | Left
    | Right

    type Direction =
    | Up
    | Down
    | Left
    | Right

    type Position = {
        Xpos : int
        Ypos : int
    }

    type Panels = Map<Position, Panel>

    type PaintState = {
        Panels : Panels
        CurrentPosition : Position
        Direction : Direction
    }

    let generateInitialPaintState () =
        let pos = { Xpos = 0; Ypos = 0; }
        let panels = Map.empty.Add (pos, { Color = Color.Black; TimesPainted = 0})
        let initialState = { Panels = panels ; CurrentPosition = pos; Direction = Direction.Up }
        initialState

    let paintPanel (paintState : PaintState) color =
        let p =  match paintState.Panels.TryFind paintState.CurrentPosition with
                 | Some panelToPaint -> { panelToPaint with Color = color; TimesPainted = panelToPaint.TimesPainted + 1 }
                 | None              -> { Color = color; TimesPainted = 1 }
        let updatedPanels = paintState.Panels.Add (paintState.CurrentPosition, p)
        { paintState with Panels = updatedPanels }

    let turnAndMove (paintState : PaintState) (turn : Turn) =
        let turnedDirection = match paintState.Direction with
                              | Up    -> if turn = Turn.Left then Direction.Left else Direction.Right
                              | Left  -> if turn = Turn.Left then Direction.Down else Direction.Up
                              | Down  -> if turn = Turn.Left then Direction.Right else Direction.Left
                              | Right -> if turn = Turn.Left then Direction.Up else Direction.Down
        let movedPosition = match turnedDirection with
                            | Up    -> { paintState.CurrentPosition with Ypos = paintState.CurrentPosition.Ypos + 1 }
                            | Left  -> { paintState.CurrentPosition with Xpos = paintState.CurrentPosition.Xpos - 1 }
                            | Down  -> { paintState.CurrentPosition with Ypos = paintState.CurrentPosition.Ypos - 1 }
                            | Right -> { paintState.CurrentPosition with Xpos = paintState.CurrentPosition.Xpos + 1 }
        { paintState with CurrentPosition = movedPosition; Direction = turnedDirection }

    let getColorOfCurrentPanel (paintState : PaintState) =
        match paintState.Panels.TryFind paintState.CurrentPosition with
        | Some panel -> panel.Color
        | None       -> Color.Black

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
        { Memory = memory; RelativeBase = 0; Inputs = [| 1L |] }
    
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
                                      printfn "Output = %d" p
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

    let parseColorOutput = function
        | 0L -> Color.Black
        | 1L -> Color.White
        | _ -> System.ArgumentException("unsupported color output") |> raise

    let parseDirectionOutput = function
        | 0L -> Turn.Left
        | 1L -> Turn.Right
        | _ -> System.ArgumentException("unsupported direction output") |> raise

    let runProgram (program : Program) =
        let mutable halt = false
        let mutable opcodePos = 0
        let mutable output = None
        let mutable outputCounter = 0
        let mutable paintState = generateInitialPaintState ()
        while not halt do
            let (opcode, parameterModes) = parseInstruction (int program.Memory.[opcodePos])
            if opcode = Halt
            then halt <- true
            else
                let parameters = Array.skip(opcodePos + 1) program.Memory |> Array.take parameterModes.Length
                System.Diagnostics.Debug.Assert(parameters.Length = parameterModes.Length)
                let ipModified, outputMade = doOpcode opcode parameterModes parameters program
                output <- outputMade
                match ipModified with
                | Some p -> opcodePos <- int p
                | None   -> opcodePos <- opcodePos + 1 + parameterModes.Length
                resizeIfNeeded program opcodePos

                if Option.isSome output
                then
                    match outputCounter with
                    | _ when (outputCounter % 2) = 0 -> let color = parseColorOutput output.Value
                                                        paintState <- paintPanel paintState color
                    | _ when (outputCounter % 2) = 1 -> let turn = parseDirectionOutput output.Value
                                                        paintState <- turnAndMove paintState turn
                                                        let input = getColorOfCurrentPanel paintState |> int64
                                                        program.Inputs <- [| input; |]
                    | _ -> System.ArgumentException("unsupported outputCounter value") |> raise
                    outputCounter <- outputCounter + 1
        paintState

    let day11 () =
        let program = getProgram InputFile
        let program' = { program with Inputs = [| 0L; |] }
        let paintState = runProgram program'
        paintState.Panels
        |> Map.filter (fun _ v -> v.TimesPainted >= 1)
        |> Map.count

    let colorToChar = function
        | Color.White -> "#"
        | Color.Black -> " "
        | _           -> System.ArgumentException("unsupported color") |> raise

    let day11Part2 () =
        let program = getProgram InputFile
        let program' = { program with Inputs = [| 1L; |] }
        let paintState = runProgram program'
        let panels = paintState.Panels |> Map.toList
        let firstLineNumber = panels |> List.maxBy (fun (p, _) -> p.Ypos) |> fun (p, _) -> p.Ypos
        let lastLineNumber = panels |> List.minBy (fun (p, _) -> p.Ypos) |> fun (p, _) -> p.Ypos
        let firstColumnNumber = panels |> List.minBy (fun (p, _) -> p.Xpos) |> fun (p, _) -> p.Xpos
        let lastColumnNumber = panels |> List.maxBy (fun (p, _) -> p.Xpos) |> fun (p, _) -> p.Xpos
        for lineNumber in [firstLineNumber .. -1 .. lastLineNumber] do
            let linePanels = panels |> List.filter (fun (p, _) -> p.Ypos = lineNumber)
                                    |> List.sortBy (fun (p, _) -> p.Xpos)
            let mutable lineString = ""
            for column in [firstColumnNumber .. lastColumnNumber] do
                let panelInColumn = linePanels |> List.tryFind (fun (p, _) -> p.Xpos = column)
                if Option.isSome panelInColumn
                then lineString <- lineString + (colorToChar (snd panelInColumn.Value).Color)
                else lineString <- lineString + " "
            printfn "%s" lineString

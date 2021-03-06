namespace Adventofcode

module Day13 =

    [<Literal>]
    let InputFile = "Day13Input.txt"

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
                                    //   printfn "Output = %d" p
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

    type OutputTriple = {
        X : Option<int>
        Y : Option<int>
        Id : Option<int>
    }

    let tileIdToString = function
        | 0 -> " "  // empty
        | 1 -> "|"  // wall
        | 2 -> "#"  // block
        | 3 -> "_"  // paddle
        | 4 -> "O"  // ball
        | _ -> System.ArgumentException("unsupported tile id") |> raise

    let printGrid (grid : Map<int * int, int>) =
        let panels = Map.toList grid
        let firstLineNumber = panels |> List.minBy (fun (p, _) -> snd p) |> fun (p, _) -> snd p
        let lastLineNumber = panels |> List.maxBy (fun (p, _) -> snd p) |> fun (p, _) -> snd p
        let firstColumnNumber = panels |> List.minBy (fun (p, _) -> fst p) |> fun (p, _) -> fst p
        let lastColumnNumber = panels |> List.maxBy (fun (p, _) -> fst p) |> fun (p, _) -> fst p
        for lineNumber in [firstLineNumber .. lastLineNumber] do
            let linePanels = panels |> List.filter (fun (p, _) -> snd p = lineNumber)
                                    |> List.sortBy (fun (p, _) -> fst p)
            let mutable lineString = ""
            for column in [firstColumnNumber .. lastColumnNumber] do
                let panelInColumn = linePanels |> List.tryFind (fun (p, _) -> fst p = column)
                if Option.isSome panelInColumn
                then lineString <- lineString + ((snd panelInColumn.Value) |> tileIdToString)
                else lineString <- lineString + " "
            printfn "%s" lineString

    let findBall (grid : Map<int * int, int>) =
        Map.tryPick (fun k v -> if v = 4 then Some k else None)  grid |> Option.get

    let findJoystick (grid : Map<int * int, int>) =
        Map.tryPick (fun k v -> if v = 3 then Some k else None)  grid |> Option.get

    let moveJoystick (grid : Map<int * int, int>) =
        let ballXPos = findBall grid |> fst
        let joystickXPos = findJoystick grid |> fst
        if ballXPos < joystickXPos
        then -1
        else if ballXPos = joystickXPos
        then 0
        else 1

    let processOutput (grid : Map<int * int, int>) outputTriple =
        if (outputTriple.X.Value = -1)
        then
            printfn "Score: %d" outputTriple.Id.Value
            grid
        else
            let grid' = Map.add (outputTriple.X.Value, outputTriple.Y.Value) outputTriple.Id.Value grid
            grid'

    let runProgram (program : Program) =
        let mutable halt = false
        let mutable opcodePos = 0
        let mutable output = None
        let mutable outputs = List.empty
        let mutable outputCounter = 0
        let mutable outputTriple = { X = None; Y = None; Id = None }
        let mutable grid = Map.empty<int * int, int>
        while not halt do
            let (opcode, parameterModes) = parseInstruction (int program.Memory.[opcodePos])
            if opcode = Halt
            then halt <- true
            else
                let parameters = Array.skip(opcodePos + 1) program.Memory |> Array.take parameterModes.Length
                System.Diagnostics.Debug.Assert(parameters.Length = parameterModes.Length)
                
                if opcode = Opcodes.Input
                then let move = moveJoystick grid
                     program.Inputs <- Array.append program.Inputs [| int64 move |]
                     printGrid grid

                let ipModified, outputMade = doOpcode opcode parameterModes parameters program
                output <- outputMade
                match ipModified with
                | Some p -> opcodePos <- int p
                | None   -> opcodePos <- opcodePos + 1 + parameterModes.Length
                resizeIfNeeded program opcodePos

                if Option.isSome output
                then
                    outputs <- outputs @ [ output.Value ]
                    match (outputCounter % 3) with
                    | 0 -> outputTriple <- { outputTriple with X = Some (int output.Value) }
                    | 1 -> outputTriple <- { outputTriple with Y = Some (int output.Value) }
                    | 2 -> outputTriple <- { outputTriple with Id = Some (int output.Value) }
                           grid <- processOutput grid outputTriple
                           outputTriple <- { X = None; Y = None; Id = None }
                    | _ -> System.Exception("bad state") |> raise
                    outputCounter <- outputCounter + 1
        grid

    let day13 () =
        let program = getProgram InputFile
        let grid = runProgram { program with Inputs = [||] }
        let count = Map.toList grid |> List.sumBy (fun (_, v) -> if v = 2 then 1 else 0)
        printGrid grid
        count
    
    let day13Part2 () =
        let program = getProgram InputFile
        program.Memory.[0] <- 2L
        runProgram program |> ignore
        ()

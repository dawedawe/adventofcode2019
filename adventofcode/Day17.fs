namespace Adventofcode

module Day17 =

    [<Literal>]
    let InputFile = "Day17Input.txt"

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

    let runProgram (program : Program) =
        let mutable halt = false
        let mutable opcodePos = 0
        let mutable output = None
        let mutable outputs = List.empty

        while not halt do
            let (opcode, parameterModes) = parseInstruction (int program.Memory.[opcodePos])
            if opcode = Halt
            then halt <- true
            else
                let parameters = Array.skip(opcodePos + 1) program.Memory |> Array.take parameterModes.Length
                System.Diagnostics.Debug.Assert(parameters.Length = parameterModes.Length)
                
                // if opcode = Opcodes.Input
                // then printfn "input needed"
                
                let ipModified, outputMade = doOpcode opcode parameterModes parameters program
                output <- outputMade
                match ipModified with
                | Some p -> opcodePos <- int p
                | None   -> opcodePos <- opcodePos + 1 + parameterModes.Length
                resizeIfNeeded program opcodePos

                if Option.isSome output
                then outputs <- outputs @ [output.Value]
        
        outputs

    type Position = {
        X : int
        Y : int
    }

    let printGrid (grid : Map<Position, char>) =
        let panels = Map.toList grid
        let firstLineNumber = panels |> List.minBy (fun (p, _) -> p.Y) |> fun (p, _) -> p.Y
        let lastLineNumber = panels |> List.maxBy (fun (p, _) -> p.Y) |> fun (p, _) -> p.Y
        let firstColumnNumber = panels |> List.minBy (fun (p, _) -> p.X) |> fun (p, _) -> p.X
        let lastColumnNumber = panels |> List.maxBy (fun (p, _) -> p.X) |> fun (p, _) -> p.X
        for lineNumber in [firstLineNumber .. lastLineNumber] do
            let linePanels = panels |> List.filter (fun (p, _) -> p.Y = lineNumber)
                                    |> List.sortBy (fun (p, _) -> p.X)
            let mutable lineString = ""
            for column in [firstColumnNumber .. lastColumnNumber] do
                let panelInColumn = linePanels |> List.tryFind (fun (p, _) -> p.X = column)
                if Option.isSome panelInColumn
                then lineString <- lineString + ((snd panelInColumn.Value) |> string)
                else lineString <- lineString + " "
            printfn "%s" lineString

    let toGrid (outputs : int64 list) =
        let mutable x = 0
        let mutable y = 0
        let mutable grid = Map.empty<Position, char>
        for i in outputs do
            match i with
            | c when c = 10L -> y <- y + 1
                                x <- 0
            | _              -> grid <- grid.Add ({ X = x; Y = y}, char i)
                                x <- x + 1
        grid

    let getNeighboursAndSelf position =
        [ position;
          { position with Y = position.Y - 1};
          { position with Y = position.Y + 1};
          { position with X = position.X - 1};
          { position with X = position.X + 1}; ]

    let isIntersection (grid : Map<Position, char>) position =
        let neighbours = getNeighboursAndSelf position
        let isInter = neighbours |> List.forall (fun p -> grid.ContainsKey p && grid.[p] = '#')
        isInter

    let getIntersectionPositions grid =
        grid |> Map.filter (fun p _ -> isIntersection grid p)
        |> Map.toList
        |> List.map fst

    let day17 () =
        let program = getProgram InputFile
        let mutable grid = runProgram program |> toGrid
        let intersections = getIntersectionPositions grid
        let sum = intersections |> List.sumBy (fun p -> p.X * p.Y)
        for i in intersections do
            grid <- grid.Add (i, 'O')
        printGrid grid
        sum

    let mainRoutine = [| 'A'; ','; 'B'; ','; 'A'; ','; 'C'; ','; 'B'; ','; 'C'; ','; 'B'; ','; 'C'; ','; 'A'; ','; 'B'; |]

    let functionA = [| 'L'; ','; '6'; ','; 'L'; ','; '4'; ','; 'R'; ','; '8'; |]

    let functionB = [| 'R'; ','; '8'; ','; 'L'; ','; '6'; ','; 'L'; ','; '4'; ','; 'L'; ','; '1'; '0' ; ','; 'R'; ','; '8'; |]

    let functionC = [| 'L'; ','; '4'; ','; 'R'; ','; '4'; ','; 'L'; ','; '4'; ','; 'R'; ','; '8'; |]

    let toAsciiInput (chars : char []) =
        let inputs = Array.map int64 chars
        Array.append inputs [| 10L |]

    let day17Part2 () =
        let program = getProgram InputFile
        program.Memory.[0] <- 2L
        let inputs = Array.concat [| (mainRoutine |> toAsciiInput);
                                     (functionA |> toAsciiInput);
                                     (functionB |> toAsciiInput);
                                     (functionC |> toAsciiInput);
                                     ([| 'n' |] |> toAsciiInput); |]
        program.Inputs <- inputs
        let dust = runProgram program
                   |> List.last
        dust

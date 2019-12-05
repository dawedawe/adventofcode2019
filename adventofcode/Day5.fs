namespace Adventofcode

module Day5 =

    [<Literal>]
    let InputFile = "Day5Input.txt"

    let InputValue = 1
    let InputValuePart2 = 5

    type ParameterMode =
    | Position  // 0
    | Immediate // 1

    type Opcodes =
    | Plus
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | Halt

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
                                     | 99 -> (Halt, 0)
                                     | _ -> System.ArgumentException("unknown opcodeCode") |> raise
        let mutable parameterModes = Array.empty
        let mutable modeNumbers = inst / 100
        for _ in 1 .. parameterCount do
            if modeNumbers > 0
            then
                let mode = match (modeNumbers % 10) with
                           | 0 -> Position
                           | 1 -> Immediate
                           | _ -> System.ArgumentException("unknown modeNumber") |> raise
                parameterModes <- Array.append parameterModes [|mode|]
                modeNumbers <- modeNumbers / 10
            else
                parameterModes <- Array.append parameterModes [|Position|]
        (opcode, parameterModes)

    let getProgram path =
        let input = System.IO.File.ReadAllLines InputFile
        let program = input.[0].Split [|','|]
                      |> Array.map System.Int32.Parse
        program
    
    let getParameter mode parameter (program : int []) =
        match mode with
        | Immediate -> parameter
        | Position -> program.[parameter]

    let doOpcode (opcode : Opcodes) (parameterModes : ParameterMode []) (parameters : int []) (program : int []) =
        match opcode with
        | Input    -> program.[parameters.[0]] <- InputValuePart2
                      None
        | Output   -> let p = getParameter parameterModes.[0] parameters.[0] program
                      printfn "Output = %d" p
                      None
        | Plus | Multiply -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                             let p2 = getParameter parameterModes.[1] parameters.[1] program
                             let op = if opcode = Plus then (+) else (*)
                             program.[parameters.[2]] <- op p1 p2
                             None
        | JumpIfTrue | JumpIfFalse -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                                      let p2 = getParameter parameterModes.[1] parameters.[1] program
                                      let op = if opcode = JumpIfTrue then (<>) else (=)
                                      if op p1 0 then Some p2 else None
        | LessThan | Equals -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                               let p2 = getParameter parameterModes.[1] parameters.[1] program
                               let op = if opcode = LessThan then (<) else (=)
                               program.[parameters.[2]] <- if op p1 p2 then 1 else 0
                               None
        | _        -> System.ArgumentException("doOpcode(): unsupported opcode") |> raise

    let runProgram (program : int []) =
        let mutable halt = false
        let mutable opcodePos = 0
        while not halt && opcodePos < (program.Length) do
            let (opcode, parameterModes) = parseInstruction program.[opcodePos] 
            if opcode = Halt
            then halt <- true
            else
                let parameters = Array.skip(opcodePos + 1) program |> Array.take parameterModes.Length
                let ipModified = doOpcode opcode parameterModes parameters program
                match ipModified with
                | Some p -> opcodePos <- p
                | None   -> opcodePos <- opcodePos + 1 + parameters.Length

    let day5 () =
        let program = getProgram InputFile
        runProgram program
        ()
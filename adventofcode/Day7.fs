namespace Adventofcode

module Day7 =

    [<Literal>]
    let InputFile = "Day7Input.txt"

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

    let doOpcode (opcode : Opcodes) (parameterModes : ParameterMode []) (parameters : int []) (program : int []) (inputValues : int []) =
        match opcode with
        | Input    -> program.[parameters.[0]] <- Array.head inputValues
                      (None, None)
        | Output   -> let p = getParameter parameterModes.[0] parameters.[0] program
                      printfn "Output = %d" p
                      (None, Some p)
        | Plus | Multiply -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                             let p2 = getParameter parameterModes.[1] parameters.[1] program
                             let op = if opcode = Plus then (+) else (*)
                             program.[parameters.[2]] <- op p1 p2
                             (None, None)
        | JumpIfTrue | JumpIfFalse -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                                      let p2 = getParameter parameterModes.[1] parameters.[1] program
                                      let op = if opcode = JumpIfTrue then (<>) else (=)
                                      if op p1 0 then (Some p2, None) else (None, None)
        | LessThan | Equals -> let p1 = getParameter parameterModes.[0] parameters.[0] program
                               let p2 = getParameter parameterModes.[1] parameters.[1] program
                               let op = if opcode = LessThan then (<) else (=)
                               program.[parameters.[2]] <- if op p1 p2 then 1 else 0
                               (None, None)
        | _        -> System.ArgumentException("doOpcode(): unsupported opcode") |> raise

    let runProgram (program : int []) (inputs : int []) =
        let mutable halt = false
        let mutable opcodePos = 0
        let mutable output = None
        let mutable inputValues = Array.copy inputs
        while not halt && opcodePos < (program.Length) do
            let (opcode, parameterModes) = parseInstruction program.[opcodePos] 
            if opcode = Halt
            then halt <- true
            else
                let parameters = Array.skip(opcodePos + 1) program |> Array.take parameterModes.Length
                let ipModified, outputMade = doOpcode opcode parameterModes parameters program inputValues
                output <- outputMade
                match ipModified with
                | Some p -> opcodePos <- p
                | None   -> opcodePos <- opcodePos + 1 + parameters.Length
                if opcode = Input
                then inputValues <- if inputValues.Length >= 2 then inputValues.[1..] else Array.empty
        if output.IsSome then output.Value else System.ArgumentException("program didn't end with output") |> raise

    let runPhaseCombination program (combination : int []) =
        let programA = Array.copy program
        let programB = Array.copy program
        let programC = Array.copy program
        let programD = Array.copy program
        let programE = Array.copy program
        let inputA = [| combination.[0]; 0 |]
        let outputA = runProgram programA inputA
        let inputB = [| combination.[1]; outputA |]
        let outputB = runProgram programB inputB
        let inputC = [| combination.[2]; outputB |]
        let outputC = runProgram programC inputC
        let inputD = [| combination.[3]; outputC |]
        let outputD = runProgram programD inputD
        let inputE = [| combination.[4]; outputD |]
        let outputE = runProgram programE inputE
        outputE

    let getPossiblePhaseCombinations () =
        let mutable combinations = System.Collections.Generic.List<int []>()
        for a in 0 .. 4 do
            for b in (Seq.filter (fun x -> x <> a) [0 .. 4]) do
                for c in (Seq.filter (fun x -> x <> a && x <> b) [0 .. 4]) do
                    for d in (Seq.filter (fun x -> x <> a && x <> b && x <> c) [0 .. 4]) do
                        for e in (Seq.filter (fun x -> x <> a && x <> b && x <> c && x <> d ) [0 .. 4]) do
                            combinations.Add [|a; b; c; d; e|]
        combinations.ToArray()


    let day7 () =
        let program = getProgram InputFile
        let combinations = getPossiblePhaseCombinations()
        let maxSignal = Array.map (runPhaseCombination program) combinations
                        |> Array.max
        maxSignal
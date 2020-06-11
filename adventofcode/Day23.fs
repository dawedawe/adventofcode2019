namespace Adventofcode

module Day23 =

    open System.Collections.Generic

    [<Literal>]
    let InputFile = "Day23Input.txt"

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
        mutable OpcodePos: int
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
        { Memory = memory; RelativeBase = 0; Inputs = Array.empty; OpcodePos = 0; }
    
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
                                      //printf "%d" p
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

    
    type Packet = { Target: int64; X: int64; Y: int64 }

    type NicState = {
        Address: int64
        Program: Program
        InputQueue: Queue<Packet>
        OutputQueue: Queue<Packet>
    }

    let distribute (nicStates: NicState []) nicState =
        let mutable r = None
        while nicState.OutputQueue.Count > 0 do
            let packet = nicState.OutputQueue.Dequeue()
            let target = int packet.Target
            if target = 255
            then r <- Some packet.Y
            else nicStates.[target].InputQueue.Enqueue(packet)
        r

    let putPacketsInInput (nicState: NicState) =
        while nicState.InputQueue.Count > 0 do
            let packet = nicState.InputQueue.Dequeue()
            let packetInputs = [| packet.X; packet.Y |]
            nicState.Program.Inputs <- Array.append nicState.Program.Inputs packetInputs

    let runProgram (nicState: NicState) =
        let mutable halt = false
        let mutable output = None
        let mutable outputs = List.empty
        let mutable awaitingInputCount = 0

        while not halt && awaitingInputCount < 1 do
            let (opcode, parameterModes) = parseInstruction (int nicState.Program.Memory.[nicState.Program.OpcodePos])
            if opcode = Halt
            then halt <- true
            else
                let parameters = Array.skip(nicState.Program.OpcodePos + 1) nicState.Program.Memory |> Array.take parameterModes.Length
                System.Diagnostics.Debug.Assert(parameters.Length = parameterModes.Length)
                
                if opcode = Opcodes.Input
                then
                    awaitingInputCount <- awaitingInputCount + 1
                    printfn "input needed by nic %d %A" nicState.Address nicState.Program.Inputs
                    if nicState.Program.Inputs.Length = 0
                    then nicState.Program.Inputs <- [| -1L |]
                
                let ipModified, outputMade = doOpcode opcode parameterModes parameters nicState.Program
                output <- outputMade
                match ipModified with
                | Some p -> nicState.Program.OpcodePos <- int p
                | None   -> nicState.Program.OpcodePos <- nicState.Program.OpcodePos + 1 + parameterModes.Length
                resizeIfNeeded nicState.Program nicState.Program.OpcodePos

                if Option.isSome output
                then
                    printfn "output %i" output.Value
                    outputs <- outputs @ [output.Value]
                    if outputs.Length = 3
                    then
                        let outPacket = { Target = outputs.[0]; X = outputs.[1]; Y = outputs.[2] }
                        nicState.OutputQueue.Enqueue(outPacket)
                        outputs <- List.empty
        
        nicState

    let copyProgram (program : Program) =
        let inputs = Array.copy program.Inputs
        let memory = Array.copy program.Memory
        let relativeBase = program.RelativeBase
        let opcodePos = program.OpcodePos
        let copy = { Inputs = inputs; Memory = memory; RelativeBase = relativeBase; OpcodePos = opcodePos }
        copy

    let getInitialStates program =
        [| 0L .. 49L |]
        |> Array.map (fun i -> { 
            Address = i
            Program = { (copyProgram program) with Inputs = [| i; |] }
            InputQueue = Queue<Packet>()
            OutputQueue = Queue<Packet>() })

    let day23 () =
        let program = getProgram InputFile
        let nicStates = getInitialStates program
        let mutable first255Y = None
        let mutable i = 0
        while Option.isNone first255Y do
            putPacketsInInput nicStates.[i]
            nicStates.[i] <- runProgram nicStates.[i]
            let r = distribute nicStates nicStates.[i]
            if Option.isNone first255Y
            then first255Y <- r
            i <- (i + 1) % 50
        first255Y.Value
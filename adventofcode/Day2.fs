namespace Adventofcode

module Day2 =

    [<Literal>]
    let InputFile = "Day2Input.txt"

    let getProgram path =
        let input = System.IO.File.ReadAllLines InputFile
        let program = input.[0].Split [|','|]
                      |> Array.map System.Int32.Parse
        program.[1] <- 12
        program.[2] <- 2
        program

    let doOpcode opcodePos inputPos1 inputPos2 outputPos (program : int []) =
        let op = if program.[opcodePos] = 1
                 then (+)
                 else if program.[opcodePos] = 2
                 then (*)
                 else System.Exception("bad opcode") |> raise
        let r = (op) program.[program.[inputPos1]] program.[program.[inputPos2]]
        program.[program.[outputPos]] <- r

    let day2 () =
        let program = getProgram InputFile
        let mutable halt = false
        let mutable opcodePos = 0
        while not halt && opcodePos < (program.Length) do
            if program.[opcodePos] = 99
            then halt <- true
            else doOpcode opcodePos (opcodePos + 1) (opcodePos + 2) (opcodePos + 3) program
            opcodePos <- opcodePos + 4
        program.[0]
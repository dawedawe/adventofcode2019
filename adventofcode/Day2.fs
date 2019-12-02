namespace Adventofcode

module Day2 =

    [<Literal>]
    let InputFile = "Day2Input.txt"

    let getProgram path =
        let input = System.IO.File.ReadAllLines InputFile
        let program = input.[0].Split [|','|]
                      |> Array.map System.Int32.Parse
        program
    
    let addNounAndVerb noun verb (program : int []) =
        program.[1] <- noun
        program.[2] <- verb

    let doOpcode opcodePos inputPos1 inputPos2 outputPos (program : int []) =
        let op = if program.[opcodePos] = 1
                 then (+)
                 else if program.[opcodePos] = 2
                 then (*)
                 else System.Exception("bad opcode") |> raise
        let r = (op) program.[program.[inputPos1]] program.[program.[inputPos2]]
        program.[program.[outputPos]] <- r

    let runProgram (program : int []) =
        let mutable halt = false
        let mutable opcodePos = 0
        while not halt && opcodePos < (program.Length) do
            if program.[opcodePos] = 99
            then halt <- true
            else doOpcode opcodePos (opcodePos + 1) (opcodePos + 2) (opcodePos + 3) program
            opcodePos <- opcodePos + 4
        program.[0]

    let day2 () =
        let program = getProgram InputFile
        addNounAndVerb 12 2 program
        runProgram program

    let day2Part2 () =
        let program = getProgram InputFile
        let mutable r = 0
        let mutable i = 0
        let combinations = Array.create (100 * 100) (0, 0)
        for n in 0 .. 99 do
            for v in 0 .. 99 do
                combinations.[i] <- (n, v)
                i <- i + 1
        
        i <- 0
        let mutable nounverb = (0, 0)
        while r <> 19690720 do
            let program' = Array.copy program
            nounverb <- combinations.[i]
            addNounAndVerb (fst nounverb) (snd nounverb) program'
            r <- runProgram program'
            i <- i + 1
        100 * (fst nounverb) + (snd nounverb)

namespace Adventofcode

module Day16 =

    [<Literal>]
    let InputFile = "Day16Input.txt"

    let getInput path =
        System.IO.File.ReadAllLines path
        |> Array.head
        |> (fun s -> s.ToCharArray())
        |> Array.map (string >> System.Int32.Parse)

    let createPattern position length =
        let basePattern = [| 0; 1; 0; -1 |]
        let r1 = Array.replicate position basePattern.[0]
        let r2 = Array.replicate position basePattern.[1]
        let r3 = Array.replicate position basePattern.[2]
        let r4 = Array.replicate position basePattern.[3]
        let pattern1 = Array.concat [| r1.[1..]; r2; r3; r4 |]
        let pattern2 = Array.concat [| r1; r2; r3; r4 |]
        let neededCopies = System.Math.Ceiling(float length / 4.0) |> int
        let pattern2Copies = Array.replicate neededCopies pattern2 |> Array.concat
        let pattern = Array.append pattern1 pattern2Copies
        pattern.[0 .. length - 1]
        |> Array.toList

    let fft (sequence : int list) =
        let mutable output = List.empty
        for p in [1 .. sequence.Length] do
            let pattern = createPattern p sequence.Length
            let pairs = List.zip sequence pattern
            let sum = pairs |> List.sumBy (fun (x, y) -> x * y)
            let digit = sum % 10 |> abs
            output <- output @ [digit]
        output
        
    let fft2 (sequence : int []) =
        let output = Array.create sequence.Length 0
        for p in [0 .. (sequence.Length - 1)] do
            if p = 0
            then
                let sum = sequence.[p..] |> Array.sum
                output.[p] <- sum
            else
                let sum = output.[p-1] - sequence.[p-1]
                output.[p] <- sum
        
        output |> Array.map (fun x -> x % 10)

    let rec doPhases n sequence =
        printfn "phase %d" n
        if n = 0
        then sequence
        else
            let sequence' = fft sequence
            doPhases (n-1) sequence'

    let rec doPhases2 n sequence =
        printfn "phase %d" n
        if n = 0
        then sequence
        else
            let sequence' = fft2 sequence
            doPhases2 (n-1) sequence'

    let day16 () =
        let input = getInput InputFile |> List.ofArray
        let output = doPhases 100 input
        output.[0..7] |> List.map string |> List.fold (+) ""

    let day16Part2 () =
        let input = getInput InputFile
        let input' = input |> Array.replicate 10000 |> Array.concat
        let offset = input'.[0 .. 6]
                     |> Array.map string
                     |> Array.fold (+) ""
                     |> int
        printfn "input length %d" input'.Length
        printfn "offset %d" offset
        let inputCut = input'.[offset ..]
        let output = doPhases2 100 inputCut
        output.[0 .. 7] |> Array.map string |> Array.fold (+) ""

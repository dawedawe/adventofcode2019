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
        let basePattern = [0; 1; 0; -1]
        let r1 = List.replicate position basePattern.[0]
        let r2 = List.replicate position basePattern.[1]
        let r3 = List.replicate position basePattern.[2]
        let r4 = List.replicate position basePattern.[3]
        let mutable pattern = r1.[1..] @ r2 @ r3 @ r4
        while pattern.Length < length do
            pattern <- pattern @ r1 @ r2 @ r3 @ r4
        pattern.[0 .. length - 1]

    let fft (sequence : int list) =
        let mutable output = List.empty
        for p in [1 .. sequence.Length] do
            let pattern = createPattern p sequence.Length
            let pairs = List.zip sequence pattern
            let sum = pairs |> List.sumBy (fun (x, y) -> x * y)
            let digit = sum % 10 |> abs
            output <- output @ [digit]
        output

    let rec doPhases n sequence =
        if n = 0
        then sequence
        else
            let sequence' = fft sequence
            doPhases (n-1) sequence'

    let day16 () =
        let input = getInput InputFile |> List.ofArray
        let output = doPhases 100 input
        output.[0..7] |> List.map string |> List.fold (+) ""

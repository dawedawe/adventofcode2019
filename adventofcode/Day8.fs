namespace Adventofcode

module Day8 =

    [<Literal>]
    let InputFile = "Day8Input.txt"

    let Tall = 6

    let Wide = 25

    let getLayers path width height =
        let layerSize = width * height
        let pixels = System.IO.File.ReadAllLines path
                     |> Array.head
                     |> (fun s -> s.ToCharArray())
                     |> Array.map (string >> System.Int32.Parse)
        let layerCount = pixels.Length / layerSize
        let mutable layers = Array.empty
        for i in 0 .. (layerCount - 1) do
            let layerStart = i * layerSize
            let layerEnd = layerStart + (layerSize - 1)
            let layer = pixels.[layerStart .. layerEnd]
            layers <- Array.append layers [| layer |]
        layers

    let digitsInLayer d =
        Array.sumBy (fun x -> if x = d then 1 else 0 )

    let day8 () =
        let layer = getLayers InputFile Wide Tall
                    |> Array.sortWith (fun l1 l2 -> compare (digitsInLayer 0 l1) (digitsInLayer 0 l2))
                    |> Array.head
        let ones = digitsInLayer 1 layer
        let twos = digitsInLayer 2 layer
        ones * twos

    let displayLayer (layer : int []) w h =
        for i in 0 .. (h - 1) do
            let rowStart = i * w
            let rowEnd = rowStart + (w - 1)
            let row = layer.[rowStart .. rowEnd]
            let symbols = Array.map (fun x -> if x = 0 then 'X' else ' ' ) row
                          |> Array.fold (fun s c -> s + string(c)) ""
            printfn "%A" symbols

    let getPixelColor (layers : int [] []) pos =
        let mutable color = 2
        let mutable i = 0
        while color = 2 && i < layers.Length do
            let pixel = layers.[i].[pos]
            match pixel with
            | 0 -> color <- 0
            | 1 -> color <- 1
            | _ -> ()
            i <- i + 1
        color


    let day8Part2 () =
        let layers = getLayers InputFile Wide Tall
        let colors = Array.map (getPixelColor layers) [| 0 .. (Tall * Wide - 1) |]
        displayLayer colors Wide Tall
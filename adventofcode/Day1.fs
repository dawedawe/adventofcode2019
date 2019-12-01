namespace Adventofcode

module Day1 =

    [<Literal>]
    let InputFile = "Day1Input.txt"

    let calcFuel mass = mass / 3 - 2

    let day1 () =
        let fuelSum = System.IO.File.ReadAllLines InputFile
                      |> Array.map System.Int32.Parse
                      |> Array.sumBy calcFuel
        fuelSum

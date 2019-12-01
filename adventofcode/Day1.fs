namespace Adventofcode

module Day1 =

    [<Literal>]
    let InputFile = "Day1Input.txt"

    let calcFuel mass = mass / 3 - 2

    let calcFuelPart2 mass =
        let mutable sum = 0
        let mutable fuel = calcFuel mass
        while fuel > 0 do
            sum <- sum + fuel
            fuel <- calcFuel fuel
        sum

    let day1 () =
        let fuelSum = System.IO.File.ReadAllLines InputFile
                      |> Array.map System.Int32.Parse
                      |> Array.sumBy calcFuel
        fuelSum

    let day1Part2 () =
        let fuelSum = System.IO.File.ReadAllLines InputFile
                      |> Array.map System.Int32.Parse
                      |> Array.sumBy calcFuelPart2
        fuelSum

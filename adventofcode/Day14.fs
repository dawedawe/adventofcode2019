namespace Adventofcode

module Day14 =

    open System.Linq

    [<Literal>]
    let InputFile = "Day14Input.txt"

    type Chemical = {
        Amount : int
        Name : string
    }

    type Reaction = {
        Inputs : Chemical list
        Output : Chemical
    }

    let parseLine (s : string) =
        let pattern = @"((\d+) ([A-Z]+))+"
        let regex = System.Text.RegularExpressions.Regex(pattern)
        let matches = regex.Matches s
        let inputs = matches.Take(matches.Count - 1)
                   |> Seq.map (fun m -> { Name = m.Groups.[3].Value; Amount = int m.Groups.[2].Value; })
                   |> Seq.toList
        let lastMatch = matches.Last()
        let output = { Name = lastMatch.Groups.[3].Value; Amount = int lastMatch.Groups.[2].Value;  }
        let reaction = { Inputs = inputs; Output = output }
        reaction

    let getReactions path =
        System.IO.File.ReadAllLines path
        |> Array.map parseLine

    let neededReactionRuns neededAmount reactionOutputAmount =
         int64 <| System.Math.Ceiling(double neededAmount / double reactionOutputAmount)

    let addOrUpdate (m : Map<string, int64>) key value =
        if m.ContainsKey key
        then m.Add (key, m.[key] + value)
        else m.Add (key, value)

    let rec traverse
        (reactions : Map<string, Reaction>)
        (chemicalToProduce : string)
        (amountToProduce : int64)
        (needsSoFar : Map<string, int64>)
        (leftOvers : Map<string, int64>)  =
        
        if (chemicalToProduce = "ORE")
        then
            (needsSoFar, leftOvers)
        else
            let mutable needsSoFar' = needsSoFar
            let mutable leftOvers' = leftOvers
            
            let reactionToDo = reactions.[chemicalToProduce]
            let amountToProduceLowerd = if leftOvers'.ContainsKey chemicalToProduce
                                        then
                                            let reducedAmount = max 0L (amountToProduce - leftOvers'.[chemicalToProduce])
                                            let reducedLeftOver = max 0L (leftOvers'.[chemicalToProduce] - amountToProduce)
                                            leftOvers' <- leftOvers'.Add (chemicalToProduce, reducedLeftOver)
                                            reducedAmount
                                        else
                                            amountToProduce

            let neededRuns = neededReactionRuns amountToProduceLowerd reactionToDo.Output.Amount
            let consumption = neededRuns * (int64 reactionToDo.Output.Amount)
            needsSoFar' <- addOrUpdate needsSoFar' chemicalToProduce consumption
            let overProduction = consumption - amountToProduceLowerd
            leftOvers' <- addOrUpdate leftOvers' chemicalToProduce overProduction
            
            for input in reactionToDo.Inputs do
                let inputProduction = (int64 input.Amount) * neededRuns
                let (x, y) = traverse reactions input.Name inputProduction needsSoFar' leftOvers'
                needsSoFar' <- x
                leftOvers' <- y
            (needsSoFar', leftOvers')

    

    let calcOreConsumption needs reactions =
        let oreProducts = reactions |> Map.filter (fun k v -> v.Inputs.[0].Name = "ORE")
        let oreNeeds = needs |> Map.filter (fun k t -> oreProducts.ContainsKey k)
        let mutable oreConsumptionSum = 0L
        for oreNeed in oreNeeds do
            let runs = neededReactionRuns oreNeed.Value reactions.[oreNeed.Key].Output.Amount
            let oreConsumption = runs * (int64 reactions.[oreNeed.Key].Inputs.[0].Amount)
            oreConsumptionSum <- oreConsumptionSum + oreConsumption
        oreConsumptionSum

    let day14 () =
        let reactions = getReactions InputFile
                        |> Array.map (fun r -> r.Output.Name, r) |> Map.ofArray
        let needs, leftOvers = traverse reactions "FUEL" 1L Map.empty Map.empty
        let oreConsumption = calcOreConsumption needs reactions
        oreConsumption
        

    let day14Part2 () =
        let reactions = getReactions InputFile
                        |> Array.map (fun r -> r.Output.Name, r) |> Map.ofArray
        let mutable oreCargo = 1000000000000L
        let mutable fuelProduced = 0L
        let mutable leftOvers = Map.empty
        let mutable fuelToProduce = 10000L
        while oreCargo > 0L do
            let needs, leftOvers' = traverse reactions "FUEL" fuelToProduce Map.empty leftOvers
            leftOvers <- leftOvers'
            let oreConsumption = calcOreConsumption needs reactions
            printfn "oreConsumption %d oreCargo %d" oreConsumption oreCargo
            if (oreCargo >= oreConsumption)
            then
                oreCargo <- oreCargo - oreConsumption
                fuelProduced <- fuelProduced + fuelToProduce
            else if fuelToProduce > 1L
            then
                printfn "Slowing down"
                fuelToProduce <- 1L
            else if fuelToProduce = 1L
            then
                oreCargo <- -1L
        fuelProduced

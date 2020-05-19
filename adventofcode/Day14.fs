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

    let day14 () =
        let reactions = getReactions InputFile
                        |> Array.map (fun r -> r.Output.Name, r) |> Map.ofArray
        let needs, leftOvers = traverse reactions "FUEL" 1L Map.empty Map.empty
        let oreProducts = reactions |> Map.filter (fun k v -> v.Inputs.[0].Name = "ORE")
        let oreNeeds = needs |> Map.filter (fun k t -> oreProducts.ContainsKey k)
        let mutable oreConsumptionSum = 0L
        for oreNeed in oreNeeds do
            let runs = neededReactionRuns oreNeed.Value reactions.[oreNeed.Key].Output.Amount
            let oreConsumption = runs * (int64 reactions.[oreNeed.Key].Inputs.[0].Amount)
            oreConsumptionSum <- oreConsumptionSum + oreConsumption
        oreConsumptionSum

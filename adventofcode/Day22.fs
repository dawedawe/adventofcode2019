namespace Adventofcode

module Day22 =

    [<Literal>]
    let InputFile = "Day22Input.txt"

    type Technique =
        | DealIntoNewStack
        | Cut of int
        | Increment of int

    let parseLine s =
        match s with
        | "deal into new stack" -> DealIntoNewStack
        | c when c.StartsWith("cut ") -> c.[4..] |> int |> Cut
        | d when d.StartsWith("deal with increment ") -> s.[20..] |> int |> Increment
        | _ -> raise (System.Exception("can't parse line: " + s))

    let getTechniques =
        System.IO.File.ReadAllLines >> Array.map parseLine

    let dealIntoNewStack = Array.rev

    let cut (cards: int []) n =
        let cutPos (cards: int []) n =
            let cutted = cards.[0..n - 1]
            Array.append cards.[n..] cutted

        let cutNeg (cards: int []) n =
            let n' = abs n
            let cutted = cards.[cards.Length - n'..]
            Array.append cutted cards.[0..cards.Length - n' - 1]

        if n < 0 then cutNeg cards n else cutPos cards n

    let increment (cards: int []) n =
        let table = Array.create cards.Length -1

        let rec helper (cards: int []) tableIdx =
            table.[tableIdx] <- cards.[0]
            if (cards.Length > 1)
            then helper cards.[1..] ((tableIdx + n) % table.Length)
            else table

        helper cards 0

    let day22 () =
        let techniques = getTechniques InputFile
        let cards = Array.init 10007 id

        let folder cards t =
            match t with
            | DealIntoNewStack -> dealIntoNewStack cards
            | Cut n -> cut cards n
            | Increment n -> increment cards n

        Array.fold folder cards techniques
        |> Array.findIndex ((=) 2019)

    let part2CardsCount = 119315717514047I
    let times = 101741582076661I

    let inv (m: bigint) (g: bigint) =
        let rec fN n i g e l a =
            match e with
            | _ when e = 0I -> g
            | _ ->
                let o = n / e
                fN e l a (n - o * e) (i - o * l) (g - o * a)

        (m + (fN m 1I 0I g 0I 1I)) % m

    let (%%%) (i: bigint) (m: bigint) = ((i % m) + m) % m

    let shuffle techniques =
        let mutable incrementMul = 1I
        let mutable offsetDiff = 0I

        for tech in techniques do
            match tech with
            | DealIntoNewStack ->
                incrementMul <- (incrementMul * -1I) %%% part2CardsCount
                offsetDiff <- (offsetDiff + incrementMul) %%% part2CardsCount
            | Cut n ->
                offsetDiff <-
                    (offsetDiff + (bigint n) * incrementMul)
                    %%% part2CardsCount
            | Increment n ->
                incrementMul <-
                    (incrementMul * inv part2CardsCount (bigint n))
                    %%% part2CardsCount
        (incrementMul, offsetDiff)

    let calcIterations (iterations: bigint) (incrementMul: bigint) (offsetDiff: bigint) =
        let increment =
            bigint.ModPow(incrementMul, iterations, part2CardsCount)

        let x = (1I - incrementMul) %%% part2CardsCount

        let offset =
            (offsetDiff
             * (1I - increment)
             * (inv part2CardsCount x))
            %%% part2CardsCount

        (increment, offset)

    let day22Part2 () =
        let techniques = getTechniques InputFile
        let position = 2020I
        let (incmul, offdiff) = shuffle techniques
        let (increment, offset) = calcIterations times incmul offdiff
        (offset + position * increment)
        %%% part2CardsCount

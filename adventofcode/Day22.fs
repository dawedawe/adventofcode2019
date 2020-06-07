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

        let rec helper (cards: int []) cardsIdx tableIdx =
            table.[tableIdx] <- cards.[0]
            if (cards.Length > 1)
            then helper cards.[1..] (cardsIdx + 1) ((tableIdx + n) % table.Length)
            else table

        helper cards 0 0

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

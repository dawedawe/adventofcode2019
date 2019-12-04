namespace Adventofcode

module Day4 =

    let lowerBound = 156218
    
    let upperBound = 652527

    let hasTwoAdjacentDigits candidate =
        let mutable found = false
        let mutable c = candidate
        while c >= 10 && not found do
            let last = c % 10
            let secondLast = c / 10 % 10
            found <- last = secondLast
            c <- c / 10
        found

    let hasExactlyTwoAdjacentDigits candidate =
        let c6 = candidate % 10
        let c5 = candidate / 10 % 10
        let c4 = candidate / 100 % 10
        let c3 = candidate / 1000 % 10
        let c2 = candidate / 10000 % 10
        let c1 = candidate / 100000 % 10
        (c1 = c2 && c2 <> c3) ||
        (c1 <> c2 && c2 = c3 && c3 <> c4) ||
        (c2 <> c3 && c3 = c4 && c4 <> c5) ||
        (c3 <> c4 && c4 = c5 && c5 <> c6) ||
        (c4 <> c5 && c5 = c6 && c5 <> c4)


    let hasIncreasingOrRepeatingDigits candidate =
        let mutable ok = true
        let mutable c = candidate
        while c >= 10 && ok do
            let last = c % 10
            let secondToLast = c / 10 % 10
            ok <- secondToLast <= last
            c <- c / 10
        ok

    let isPasswordCandidate = fun c -> hasTwoAdjacentDigits c && hasIncreasingOrRepeatingDigits c

    let isPasswordCandidatePart2 = fun c -> hasExactlyTwoAdjacentDigits c && hasIncreasingOrRepeatingDigits c

    let getPasswordsCount filter lower upper =
        Seq.filter filter [lower .. upper]
        |> Seq.length

    let day4 () =
        let passwords = getPasswordsCount isPasswordCandidate lowerBound upperBound
        passwords

    let day4Part2 () =
        let passwords = getPasswordsCount isPasswordCandidatePart2 lowerBound upperBound
        passwords

namespace Adventofcode

module Day4 =

    let lowerBound = 156218
    
    let upperBound = 652527

    let hasTwoAdjacentDigits candidate =
        let mutable found = false
        let mutable c = candidate
        while c >= 10 && not found do
            let last = c % 10
            let secondToLast = c / 10 % 10
            found <- last = secondToLast
            c <- c / 10
        found

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

    let getPasswordsCount lower upper =
        Seq.filter (isPasswordCandidate) [lower .. upper]
        |> Seq.length

    let day4 () =
        let passwords = getPasswordsCount lowerBound upperBound
        passwords

namespace Adventofcode

module Day12 =

    [<Literal>]
    let InputFile = "Day12Input.txt"

    type Moon = {
            Xpos : int
            Ypos : int
            Zpos : int
            Xvel : int
            Yvel : int
            Zvel : int
    }

    let parseLine (line : string) =
        let pattern = @"\<x=(-?\d+), y=(-?\d+), z=(-?\d+)\>"
        let regex = System.Text.RegularExpressions.Regex(pattern)
        let m = regex.Match(line)
        {
            Xpos = int(m.Groups.[1].Value)
            Ypos = int(m.Groups.[2].Value)
            Zpos = int(m.Groups.[3].Value)
            Xvel = 0
            Yvel = 0
            Zvel = 0
        }

    let getMoons path =
        System.IO.File.ReadAllLines path
        |> Array.map parseLine

    let getGravityChange m1Pos m2Pos =
        match (m1Pos, m2Pos) with
        | _ when m1Pos < m2Pos -> 1
        | _ when m1Pos > m2Pos -> -1
        | _                    -> 0

    let applyGravity m1 m2 m3 m4 =
        let (x2Delta, x3Delta, x4Delta) = (getGravityChange m1.Xpos m2.Xpos), (getGravityChange m1.Xpos m3.Xpos), (getGravityChange m1.Xpos m4.Xpos)
        let (y2Delta, y3Delta, y4Delta) = (getGravityChange m1.Ypos m2.Ypos), (getGravityChange m1.Ypos m3.Ypos), (getGravityChange m1.Ypos m4.Ypos)
        let (z2Delta, z3Delta, z4Delta) = (getGravityChange m1.Zpos m2.Zpos), (getGravityChange m1.Zpos m3.Zpos), (getGravityChange m1.Zpos m4.Zpos)
        let xDelta = x2Delta + x3Delta + x4Delta
        let yDelta = y2Delta + y3Delta + y4Delta
        let zDelta = z2Delta + z3Delta + z4Delta
        let m1' = { m1 with Xvel = m1.Xvel + xDelta; Yvel = m1.Yvel + yDelta; Zvel = m1.Zvel + zDelta }
        m1'

    let applyVelocity m =
        let m' = { m with Xpos = m.Xpos + m.Xvel; Ypos = m.Ypos + m.Yvel; Zpos = m.Zpos + m.Zvel; }
        m'

    let doStep m1 m2 m3 m4 =
        let m1' = applyGravity m1 m2 m3 m4 |> applyVelocity
        let m2' = applyGravity m2 m1 m3 m4 |> applyVelocity
        let m3' = applyGravity m3 m1 m2 m4 |> applyVelocity
        let m4' = applyGravity m4 m1 m2 m3 |> applyVelocity
        (m1', m2', m3', m4')

    let rec runNSteps n m1 m2 m3 m4 =
        if n = 0 then (m1, m2, m3, m4)
        else
            let (m1', m2', m3', m4') = doStep m1 m2 m3 m4
            let n' = n - 1
            runNSteps n' m1' m2' m3' m4'

    let calcPotentialEnergy m =
        let potentialEnergy = abs m.Xpos + abs m.Ypos + abs m.Zpos
        potentialEnergy

    let calcKineticEnergy m =
        let potentialEnergy = abs m.Xvel + abs m.Yvel + abs m.Zvel
        potentialEnergy

    let calcMoonTotalEnergy m =
        let potentialEnergy = calcPotentialEnergy m
        let kineticEnergy = calcKineticEnergy m
        let totalEnergy = potentialEnergy * kineticEnergy
        totalEnergy

    let day12 () =
        let moons = getMoons InputFile
        let (m1, m2, m3, m4) = (moons.[0], moons.[1], moons.[2], moons.[3])
        let (m1', m2', m3', m4') = runNSteps 1000 m1 m2 m3 m4
        let moons' = [| m1'; m2'; m3'; m4'; |]
        let totalEnergy = Array.sumBy calcMoonTotalEnergy moons'
        totalEnergy

    let rec runTillRepeat
            m1 m2 m3 m4 
            (xStates : Set<int * int * int * int * int * int * int * int>)
            (yStates : Set<int * int * int * int * int * int * int * int>)
            (zStates : Set<int * int * int * int * int * int * int * int>) =
        let (m1', m2', m3', m4') = doStep m1 m2 m3 m4
        let xStateToAdd = (m1'.Xpos, m2'.Xpos, m3'.Xpos, m4'.Xpos, m1'.Xvel, m2'.Xvel, m3'.Xvel, m4'.Xvel)
        let yStateToAdd = (m1'.Ypos, m2'.Ypos, m3'.Ypos, m4'.Ypos, m1'.Yvel, m2'.Yvel, m3'.Yvel, m4'.Yvel)
        let zStateToAdd = (m1'.Zpos, m2'.Zpos, m3'.Zpos, m4'.Zpos, m1'.Zvel, m2'.Zvel, m3'.Zvel, m4'.Zvel)

        if xStates.Contains xStateToAdd && yStates.Contains yStateToAdd && zStates.Contains zStateToAdd
        then
            (xStates.Count, yStates.Count, zStates.Count)
        else
            let xStates' = Set.add xStateToAdd xStates
            let yStates' = Set.add yStateToAdd yStates
            let zStates' = Set.add zStateToAdd zStates
            runTillRepeat m1' m2' m3' m4' xStates' yStates' zStates'

    let rec gcd a b = if b = 0L then a else gcd b (a % b)

    let lcm a b = (a * b) / (gcd a b)

    let day12Part2 () =
        let moons = getMoons InputFile
        let (m1, m2, m3, m4) = (moons.[0], moons.[1], moons.[2], moons.[3])
        let xStates = Set.singleton (m1.Xpos, m2.Xpos, m3.Xpos, m4.Xpos, m1.Xvel, m2.Xvel, m3.Xvel, m4.Xvel)
        let yStates = Set.singleton (m1.Ypos, m2.Ypos, m3.Ypos, m4.Ypos, m1.Yvel, m2.Yvel, m3.Yvel, m4.Yvel)
        let zStates = Set.singleton (m1.Zpos, m2.Zpos, m3.Zpos, m4.Zpos, m1.Zvel, m2.Zvel, m3.Zvel, m4.Zvel)
        let (xCount, yCount, zCount) = runTillRepeat m1 m2 m3 m4 xStates yStates zStates
        let r = lcm (lcm (int64 xCount) (int64 yCount)) (int64 zCount)
        r

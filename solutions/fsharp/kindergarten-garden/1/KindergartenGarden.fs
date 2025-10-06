module KindergartenGarden

// TODO: define the Plant type
type Plant =
    | Grass
    | Clover
    | Radishes
    | Violets

let plants (diagram: string) (student: string) =
    let index = student[0] - 'A' |> int
    let lines = diagram.Split('\n')

    [ lines[0][index * 2]
      lines[0][index * 2 + 1]
      lines[1][index * 2]
      lines[1][index * 2 + 1] ]
    |> List.map (function
        | 'G' -> Grass
        | 'C' -> Clover
        | 'R' -> Radishes
        | 'V' -> Violets)

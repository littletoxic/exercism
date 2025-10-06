module TisburyTreasureHunt

let getCoordinate (line: string * string) : string = snd line

let convertCoordinate (coordinate: string) : int * char =
    System.Convert.ToInt32(coordinate[0..0]), coordinate[1]

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool =
    let azarasCoordinate = azarasData |> getCoordinate |> convertCoordinate

    let _, ruisCoordinate, _ = ruisData

    azarasCoordinate = ruisCoordinate

let createRecord
    (azarasData: string * string)
    (ruisData: string * (int * char) * string)
    : (string * string * string * string) =
    match compareRecords azarasData ruisData with
    | true ->
        let location, _, quadrant = ruisData
        let treasure, coordinate = azarasData

        coordinate, location, quadrant, treasure
    | false -> "", "", "", ""

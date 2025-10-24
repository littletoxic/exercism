module HighScores

let scores (values: int list) : int list = values

let latest (values: int list) : int = List.last values

let personalBest (values: int list) : int = List.max values

let personalTopThree (values: int list) : int list =
    List.sortDescending values |> List.truncate 3

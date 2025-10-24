module HighScores

let scores (values: int list) : int list = values

let latest (values: int list) : int = List.last values

let personalBest (values: int list) : int = List.max values

let personalTopThree (values: int list) : int list =
    let sorted = List.sortDescending values

    if List.length sorted > 3 then
        List.take 3 sorted
    else
        sorted

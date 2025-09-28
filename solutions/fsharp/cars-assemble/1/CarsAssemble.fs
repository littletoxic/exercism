module CarsAssemble

let successRate (speed: int) : float =
    if speed <= 0 then 0
    elif speed <= 4 then 1
    elif speed <= 8 then 0.9
    elif speed <= 9 then 0.8
    elif speed <= 10 then 0.77
    else 0

let productionRatePerHour (speed: int) : float =
    successRate speed * float speed * float 221

let workingItemsPerMinute (speed: int) : int =
    productionRatePerHour speed |> int |> (/) <| 60

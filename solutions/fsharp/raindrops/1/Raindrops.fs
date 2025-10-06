module Raindrops

let convert (number: int) : string =
    let result = System.Text.StringBuilder()

    if number % 3 = 0 then
        result.Append "Pling" |> ignore

    if number % 5 = 0 then
        result.Append "Plang" |> ignore

    if number % 7 = 0 then
        result.Append "Plong" |> ignore

    let str = result.ToString()
    if str = "" then string number else str

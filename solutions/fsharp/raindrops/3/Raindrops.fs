module Raindrops

let toFunc (factor, label) (i, labelSoFar) =
    if i % factor = 0 then
        i, Some(Option.defaultValue "" labelSoFar + label)
    else
        i, labelSoFar

let convert (number: int) : string =
    (number, None)
    |> ([ 3, "Pling"; 5, "Plang"; 7, "Plong" ] |> List.map toFunc |> List.fold (>>) id)
    |> snd
    |> Option.defaultValue (string number)

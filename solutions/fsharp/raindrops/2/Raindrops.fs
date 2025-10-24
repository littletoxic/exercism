module Raindrops

let convert (number: int) : string =
    [ 3, "Pling"; 5, "Plang"; 7, "Plong" ]
    |> List.choose (fun (x, str) -> if number % x = 0 then Some str else None)
    |> function
        | [] -> string number
        | strings -> String.concat "" strings

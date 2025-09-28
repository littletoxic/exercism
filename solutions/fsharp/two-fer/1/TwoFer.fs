module TwoFer

let twoFer (input: string option) : string =
    match input with
    | Some value -> sprintf "One for %s, one for me." value
    | None -> "One for you, one for me."

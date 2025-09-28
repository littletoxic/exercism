module Bob

let isQuestion (input: string) = 
    input.TrimEnd().EndsWith('?')

let isYelling (input: string) = 
    input = input.ToUpper() && 
    input <> input.ToLower() // 存在至少一个 letter

let isSilence (input: string) = 
    System.String.IsNullOrWhiteSpace(input)

let response (input: string): string = 
    match input with
    | x when isSilence x -> "Fine. Be that way!"
    | x when isYelling x && isQuestion x -> "Calm down, I know what I'm doing!"
    | x when isQuestion x -> "Sure."
    | x when isYelling x -> "Whoa, chill out!"
    | _ -> "Whatever."
module Bob

let (|Question|_|) (input: string) =
    if input.TrimEnd().EndsWith('?') then Some() else None

let (|Yelling|_|) (input: string) =
    if input = input.ToUpper() && 
        input <> input.ToLower() // 存在至少一个 letter
    then
        Some()
    else
        None

let (|Silence|_|) (input: string) =
    if System.String.IsNullOrWhiteSpace(input) then
        Some()
    else
        None

let response (input: string) : string =
    match input with
    | Silence -> "Fine. Be that way!"
    | Yelling & Question -> "Calm down, I know what I'm doing!"
    | Question -> "Sure."
    | Yelling -> "Whoa, chill out!"
    | _ -> "Whatever."

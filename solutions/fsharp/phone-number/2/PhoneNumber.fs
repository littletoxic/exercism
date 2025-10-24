module PhoneNumber

let clean input =

    let validateNoLetters input =
        if input |> String.exists System.Char.IsLetter then
            Error "letters not permitted"
        else
            Ok input

    let validateNoPunctuation input =
        let invalidPunctuation c =
            System.Char.IsPunctuation c && not (List.contains c [ '-'; '.'; '('; ')'; '+' ])

        if input |> String.exists invalidPunctuation then
            Error "punctuations not permitted"
        else
            Ok input

    let validateLength digits =
        match String.length digits with
        | len when len < 10 -> Error "incorrect number of digits"
        | 10 -> Ok digits
        | 11 when digits[0] = '1' -> Ok(digits.Substring(1))
        | 11 -> Error "11 digits must start with 1"
        | _ -> Error "more than 11 digits"

    let validateAreaCode (digits: string) =
        match digits[0] with
        | '0' -> Error "area code cannot start with zero"
        | '1' -> Error "area code cannot start with one"
        | _ -> Ok digits

    let validateExchangeCode (digits: string) =
        match digits[3] with
        | '0' -> Error "exchange code cannot start with zero"
        | '1' -> Error "exchange code cannot start with one"
        | _ -> Ok digits

    // 函数组合管道
    validateNoLetters input
    |> Result.bind validateNoPunctuation
    |> Result.map (String.filter System.Char.IsDigit)
    |> Result.bind validateLength
    |> Result.bind validateAreaCode
    |> Result.bind validateExchangeCode
    |> Result.map System.UInt64.Parse

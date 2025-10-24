module PhoneNumber

let clean input =

    let validateNoLetters input =
        if input |> Seq.exists System.Char.IsLetter then
            Error "letters not permitted"
        else
            Ok input

    let validateNoPunctuation input =
        let hasInvalidPunctuation =
            input
            |> Seq.exists (fun c ->
                not (System.Char.IsDigit c)
                && not (System.Char.IsWhiteSpace c)
                && not (List.contains c [ '-'; '.'; '('; ')'; '+' ]))

        if hasInvalidPunctuation then
            Error "punctuations not permitted"
        else
            Ok input

    let extractDigits input =
        input
        |> Seq.filter System.Char.IsDigit
        |> Seq.map string
        |> String.concat ""
        |> Ok

    let validateLength digits =
        match String.length digits with
        | len when len < 10 -> Error "incorrect number of digits"
        | 10 -> Ok digits
        | 11 when digits[0] = '1' -> Ok(digits.Substring(1))
        | 11 -> Error "11 digits must start with 1"
        | _ -> Error "more than 11 digits"

    let validateAreaCode (digits: string) =
        match digits.[0] with
        | '0' -> Error "area code cannot start with zero"
        | '1' -> Error "area code cannot start with one"
        | _ -> Ok digits

    let validateExchangeCode (digits: string) =
        match digits[3] with
        | '0' -> Error "exchange code cannot start with zero"
        | '1' -> Error "exchange code cannot start with one"
        | _ -> Ok digits

    let parseNumber digits = Ok(System.UInt64.Parse digits)

    // 函数组合管道
    input
    |> validateNoLetters
    |> Result.bind validateNoPunctuation
    |> Result.bind extractDigits
    |> Result.bind validateLength
    |> Result.bind validateAreaCode
    |> Result.bind validateExchangeCode
    |> Result.bind parseNumber

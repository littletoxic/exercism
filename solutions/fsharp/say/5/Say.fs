module Say

// TODO: implement this module

let private ones =
    [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

let private teens =
    [| "ten"
       "eleven"
       "twelve"
       "thirteen"
       "fourteen"
       "fifteen"
       "sixteen"
       "seventeen"
       "eighteen"
       "nineteen" |]

let private tens =
    [| ""
       ""
       "twenty"
       "thirty"
       "forty"
       "fifty"
       "sixty"
       "seventy"
       "eighty"
       "ninety" |]

let private scales = [| ""; "thousand"; "million"; "billion" |]

let private sayTwoDigits =
    function
    | n when n < 10 -> ones[n]
    | n when n < 20 -> teens[n - 10]
    | n ->
        [ tens[n / 10]; ones[n % 10] ]
        |> List.filter (System.String.IsNullOrEmpty >> not)
        |> String.concat "-"

let private sayHundreds =
    function
    | n when n < 100 -> [ sayTwoDigits n ]
    | n -> [ ones[n / 100]; "hundred"; sayTwoDigits (n % 100) ]

let say =
    function
    | n when n < 0L || n > 999_999_999_999L -> None
    | 0L -> Some "zero"
    | n ->
        n
        |> Seq.unfold (function
            | 0L -> None
            | state -> Some(state % 1000L |> int, state / 1000L))
        |> Seq.indexed
        |> Seq.rev
        |> Seq.collect (function
            | _, 0 -> Seq.empty
            | scale, chunk -> sayHundreds chunk @ [ scales[scale] ])
        |> Seq.filter (System.String.IsNullOrEmpty >> not)
        |> String.concat " "
        |> Some

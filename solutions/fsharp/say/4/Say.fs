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
    | n when n < 10L -> ones[int n]
    | n when n < 20L -> teens[int n - 10]
    | n ->
        [ tens[int n / 10]; ones[int n % 10] ]
        |> List.filter (System.String.IsNullOrEmpty >> not)
        |> String.concat "-"

let private sayHundreds =
    function
    | n when n < 100L -> [ sayTwoDigits n ]
    | n -> [ $"{ones[int n / 100]} hundred"; sayTwoDigits (n % 100L) ]

let say =
    function
    | n when n < 0L || n > 999_999_999_999L -> None
    | 0L -> Some "zero"
    | n ->
        n
        |> Seq.unfold (function
            | 0L -> None
            | state -> Some(state % 1000L, state / 1000L))
        |> Seq.indexed
        |> Seq.rev
        |> Seq.collect (function
            | _, 0L -> Seq.empty
            | scale, chunk -> sayHundreds chunk @ [ scales[scale] ])
        |> Seq.filter (System.String.IsNullOrEmpty >> not)
        |> String.concat " "
        |> Some

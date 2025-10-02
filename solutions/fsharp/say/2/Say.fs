module Say

// TODO: implement this module
open type System.Math

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

let private sayTwoDigits n =
    match n with
    | _ when n < 10L -> ones[int n]
    | _ when n < 20L -> teens[int (n - 10L)]
    | _ ->
        let t, o = DivRem(n, 10L)

        [ tens[int t]; ones[int o] ]
        |> List.filter (System.String.IsNullOrEmpty >> not)
        |> String.concat "-"

let private sayHundreds n =
    match n with
    | 0L -> []
    | n when n < 100L -> [ sayTwoDigits n ]
    | n ->
        let h, rest = DivRem(n, 100L)

        [ yield $"{ones[int h]} hundred"
          if rest > 0L then
              yield sayTwoDigits rest ]

let private sayThousands n =
    let rec loop num scale acc =
        match num with
        | 0L -> acc
        | _ ->
            let rest, chunk = DivRem(num, 1000L)

            let parts =
                match chunk with
                | 0L -> acc
                | _ ->
                    let chunkWords = sayHundreds chunk |> String.concat " "

                    let scaled =
                        match scale with
                        | 0 -> chunkWords
                        | _ -> $"{chunkWords} {scales[scale]}"

                    scaled :: acc

            loop rest (scale + 1) parts

    loop n 0 []

let say (number: int64) =
    match number with
    | _ when number < 0L || number > 999_999_999_999L -> None
    | 0L -> Some "zero"
    | _ -> sayThousands number |> String.concat " " |> Some

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

let private sayTwoDigits =
    function
    | n when n < 10L -> ones[int n]
    | n when n < 20L -> teens[int (n - 10L)]
    | n ->
        let t, o = DivRem(n, 10L)

        [ tens[int t]; ones[int o] ]
        |> List.filter (System.String.IsNullOrEmpty >> not)
        |> String.concat "-"

let private sayHundreds =
    function
    | 0L -> []
    | n when n < 100L -> [ sayTwoDigits n ]
    | n ->
        let h, rest = DivRem(n, 100L)

        [ $"{ones[int h]} hundred"; sayTwoDigits rest ]


let private sayThousands n =
    let rec loop num scale acc =
        if num = 0L then
            acc
        else
            let rest, chunk = DivRem(num, 1000L)

            match chunk with
            | 0L -> loop rest (scale + 1) acc
            | _ ->
                let parts = sayHundreds chunk @ [ scales[scale] ] @ acc

                loop rest (scale + 1) parts

    loop n 0 []

let say =
    function
    | n when n < 0L || n > 999_999_999_999L -> None
    | 0L -> Some "zero"
    | n ->
        sayThousands n
        |> List.filter (System.String.IsNullOrEmpty >> not)
        |> String.concat " "
        |> Some

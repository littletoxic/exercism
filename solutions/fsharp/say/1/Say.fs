module Say

// TODO: implement this module

let getSingleDigit number =
    match number with
    | 0L -> ""
    | 1L -> "one"
    | 2L -> "two"
    | 3L -> "three"
    | 4L -> "four"
    | 5L -> "five"
    | 6L -> "six"
    | 7L -> "seven"
    | 8L -> "eight"
    | 9L -> "nine"
    | _ -> ""

let getMoreThanTenLessThanTwenty number =
    match number with
    | 10L -> "ten"
    | 11L -> "eleven"
    | 12L -> "twelve"
    | 13L -> "thirteen"
    | 14L -> "fourteen"
    | 15L -> "fifteen"
    | 16L -> "sixteen"
    | 17L -> "seventeen"
    | 18L -> "eighteen"
    | 19L -> "nineteen"
    | _ -> ""

let getTenDigit prefix =
    match prefix with
    | 2L -> "twenty"
    | 3L -> "thirty"
    | 4L -> "forty"
    | 5L -> "fifty"
    | 6L -> "sixty"
    | 7L -> "seventy"
    | 8L -> "eighty"
    | 9L -> "ninety"
    | _ -> ""

let getThousandLevelSuffix layer =
    match layer with
    | 0 -> ""
    | 1 -> "thousand"
    | 2 -> "million"
    | 3 -> "billion"
    | _ -> ""

let getMoreThanTwentyLessThanThousand number =
    let quotient, remainder = System.Math.DivRem(number, 10L)
    let tenDigit = getTenDigit quotient

    match remainder with
    | 0L -> tenDigit
    | _ -> tenDigit + "-" + getSingleDigit remainder

let processLessThanThousand number =
    let quotient, remainder = System.Math.DivRem(number, 100L)

    let tenDigit =
        match remainder with
        | _ when remainder < 10 -> getSingleDigit remainder
        | _ when remainder < 20 -> getMoreThanTenLessThanTwenty remainder
        | _ -> getMoreThanTwentyLessThanThousand remainder

    (match quotient with
     | 0L -> tenDigit
     | _ -> getSingleDigit quotient + " " + "hundred" + " " + tenDigit)
        .Trim()


let rec sayHelper number layer =
    let quotient, remainder = System.Math.DivRem(number, 1000L)

    let tail =
        match remainder with
        | 0L -> ""
        | _ -> processLessThanThousand remainder + " " + getThousandLevelSuffix layer

    (match quotient with
     | 0L -> tail
     | _ -> sayHelper quotient (layer + 1) + " " + tail)
        .Trim()

let say (number: int64) =
    match number with
    | 0L -> Some "zero"
    | _ when number < 0 -> None
    | _ when number > 999_999_999_999L -> None
    | _ -> Some(sayHelper number 0)

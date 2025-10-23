module TwelveDays

let ordinal =
    [ ""
      "first"
      "second"
      "third"
      "fourth"
      "fifth"
      "sixth"
      "seventh"
      "eighth"
      "ninth"
      "tenth"
      "eleventh"
      "twelfth" ]

let suffixes =
    [ ""
      "a Partridge in a Pear Tree"
      "two Turtle Doves"
      "three French Hens"
      "four Calling Birds"
      "five Gold Rings"
      "six Geese-a-Laying"
      "seven Swans-a-Swimming"
      "eight Maids-a-Milking"
      "nine Ladies Dancing"
      "ten Lords-a-Leaping"
      "eleven Pipers Piping"
      "twelve Drummers Drumming" ]

let getPrefix day =
    $"On the {ordinal[day]} day of Christmas my true love gave to me: "

let getSuffix day =
    [ 1..day ]
    |> List.fold
        (fun tail i ->
            match i with
            | 1 -> suffixes[i]
            | 2 -> $"{suffixes[i]}, and {tail}"
            | _ -> $"{suffixes[i]}, {tail}")
        ""

let getSentence day = getPrefix day + getSuffix day + "."

let recite start stop = [ start..stop ] |> List.map getSentence

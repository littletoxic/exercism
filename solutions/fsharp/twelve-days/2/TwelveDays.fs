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
    [ "a Partridge in a Pear Tree"
      "and a Partridge in a Pear Tree"
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

let getSuffix =
    function
    | 1 -> suffixes[0]
    | n -> [ 1..n ] |> List.rev |> List.map (fun i -> suffixes[i]) |> String.concat ", "

let getSentence day = getPrefix day + getSuffix day + "."

let recite start stop = [ start..stop ] |> List.map getSentence

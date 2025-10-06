module Clock

type Clock = { Hours: int; Minutes: int }

let create hours minutes =
    let minutesInDay = (minutes % (24 * 60) + 24 * 60) % (24 * 60)
    let formattedMinutes = minutesInDay % 60
    let hoursInDay = (hours % 24 + 24) % 24
    let formattedHours = (hoursInDay + minutesInDay / 60) % 24

    { Hours = formattedHours
      Minutes = formattedMinutes }

let add minutes clock =
    let allMinutes = clock.Minutes + minutes
    let formattedMinutes = allMinutes % 60
    let formattedHours = (clock.Hours + allMinutes / 60) % 24
    create formattedHours formattedMinutes

let subtract minutes clock =
    let addMinutes = 24 * 60 - minutes % (24 * 60)
    add addMinutes clock


let display clock = $"{clock.Hours:D2}:{clock.Minutes:D2}"

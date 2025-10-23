module Pangram

let isPangram (input: string) : bool =
    set [ 'a' .. 'z' ] - set (input.ToLower()) |> Set.isEmpty

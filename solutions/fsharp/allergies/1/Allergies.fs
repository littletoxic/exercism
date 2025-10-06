module Allergies

open System

// TODO: define the Allergen type
[<Flags>]
type Allergen =
    | Eggs = (1 <<< 0)
    | Peanuts = (1 <<< 1)
    | Shellfish = (1 <<< 2)
    | Strawberries = (1 <<< 3)
    | Tomatoes = (1 <<< 4)
    | Chocolate = (1 <<< 5)
    | Pollen = (1 <<< 6)
    | Cats = (1 <<< 7)

let allergicTo codedAllergies allergen =
    let value = enum<Allergen> codedAllergies
    value.HasFlag(allergen)

let list codedAllergies =
    let value = enum<Allergen> codedAllergies

    Allergen.GetValues<Allergen>()
    |> List.ofArray
    |> List.filter (fun e -> value.HasFlag(e))

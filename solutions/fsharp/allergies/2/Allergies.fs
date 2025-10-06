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
    enum<Allergen>(codedAllergies).HasFlag(allergen)

let list codedAllergies =
    Allergen.GetValues<Allergen>()
    |> List.ofArray
    |> List.filter (allergicTo codedAllergies)

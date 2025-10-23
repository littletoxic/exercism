module Pangram

let isPangram (input: string): bool = 
    input
    |> Seq.where System.Char.IsAsciiLetter
    |> Seq.distinctBy System.Char.ToLower
    |> Seq.length = 26
module Isogram

let isIsogram (str: string): bool = 
    str
    |> Seq.where System.Char.IsAsciiLetter
    |> Seq.countBy System.Char.ToLower
    |> Seq.forall (snd >> (=) 1)
module PigLatin

let vowels = set [ 'a'; 'e'; 'i'; 'o'; 'u' ]

let (|StartsWithVowelOrSpecial|_|) (word: string) =
    if vowels.Contains(word[0]) || word.StartsWith("xr") || word.StartsWith("yt") then
        Some StartsWithVowelOrSpecial
    else
        None

let (|SplitAt|_|) (word: string) =
    let vowelIndex = word |> Seq.tryFindIndex vowels.Contains

    let yIndex = word.IndexOf('y', 1)
    let quIndex = word.IndexOf("qu")

    if
        quIndex >= 0
        && (vowelIndex.IsNone || quIndex < vowelIndex.Value)
        && (yIndex < 0 || quIndex < yIndex)
    then
        SplitAt(quIndex + 2) |> Some
    elif yIndex > 0 && (vowelIndex.IsNone || yIndex < vowelIndex.Value) then
        SplitAt yIndex |> Some
    elif vowelIndex.IsSome then
        SplitAt vowelIndex.Value |> Some
    else
        None

let translateWord (word: string) =
    match word with
    | StartsWithVowelOrSpecial -> word + "ay"
    | SplitAt index when index < word.Length -> word[index..] + word[.. index - 1] + "ay"
    | _ -> word + "ay"

let translate (input: string) =
    input.Split(' ') |> Array.map translateWord |> String.concat " "

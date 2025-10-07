module Hamming

let distance (strand1: string) (strand2: string) : int option =
    match String.length strand1 = String.length strand2 with
    | true ->
        strand1
        |> Seq.zip strand2
        |> Seq.filter (fun (c1, c2) -> c1 <> c2)
        |> Seq.length
        |> Some
    | false -> None

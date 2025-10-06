module OcrNumbers

let convertLine line =
    line
    |> List.map (Seq.chunkBySize 3 >> Seq.map System.String >> List.ofSeq)
    |> List.transpose
    |> List.map (function
        | [ " _ "; "| |"; "|_|"; "   " ] -> "0"
        | [ "   "; "  |"; "  |"; "   " ] -> "1"
        | [ " _ "; " _|"; "|_ "; "   " ] -> "2"
        | [ " _ "; " _|"; " _|"; "   " ] -> "3"
        | [ "   "; "|_|"; "  |"; "   " ] -> "4"
        | [ " _ "; "|_ "; " _|"; "   " ] -> "5"
        | [ " _ "; "|_ "; "|_|"; "   " ] -> "6"
        | [ " _ "; "  |"; "  |"; "   " ] -> "7"
        | [ " _ "; "|_|"; "|_|"; "   " ] -> "8"
        | [ " _ "; "|_|"; " _|"; "   " ] -> "9"
        | _ -> "?")
    |> String.concat ""

let convert input =
    if
        List.length input % 4 = 0
        && input |> List.forall (fun str -> String.length str % 3 = 0)
    then
        input |> List.chunkBySize 4 |> List.map convertLine |> String.concat "," |> Some
    else
        None

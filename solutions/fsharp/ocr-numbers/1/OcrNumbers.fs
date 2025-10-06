module OcrNumbers

let convert input =
    if
        List.length input % 4 = 0
        && input |> List.forall (fun str -> String.length str % 3 = 0)
    then
        input
        |> List.unfold (fun left ->
            match List.length left >= 4 with
            | true -> Some(left[0..3], left[4..])
            | false -> None)
        |> List.map (
            List.unfold (fun left ->
                match left |> List.forall (fun str -> String.length str >= 3) with
                | true -> Some(left |> List.map (fun str -> str[0..2]), left |> List.map (fun str -> str[3..]))
                | false -> None)
            >> List.map (function
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
            >> String.concat ""
        )
        |> String.concat ","
        |> Some
    else
        None

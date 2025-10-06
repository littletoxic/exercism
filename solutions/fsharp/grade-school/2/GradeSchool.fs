module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School) : School =
    match school |> Map.exists (fun _ students -> students |> List.contains student) with
    | true -> school
    | false ->
        let cons x xs = x :: xs

        school
        |> Map.change grade (Option.defaultValue [] >> cons student >> List.sort >> Some)


let roster (school: School) : string list =
    school |> Map.toList |> List.sortBy fst |> List.collect snd

let grade (number: int) (school: School) : string list =
    school |> Map.tryFind number |> Option.defaultValue []

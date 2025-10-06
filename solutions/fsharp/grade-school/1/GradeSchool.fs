module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School) : School =
    if school |> Map.exists (fun _ students -> students |> Seq.contains student) then
        school
    else
        school
        |> Map.change grade (function
            | Some students -> Some(student :: students |> List.sort)
            | None -> Some [ student ])


let roster (school: School) : string list =
    school |> Map.toList |> List.sortBy fst |> List.collect snd

let grade (number: int) (school: School) : string list =
    match school |> Map.tryFind number with
    | Some students -> students
    | None -> []

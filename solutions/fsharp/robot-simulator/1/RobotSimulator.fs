module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot = Direction * Position

let create direction position = direction, position

let move instructions robot =
    let moveOne robot instruction =
        match instruction with
        | 'R' ->
            match robot with
            | North, pos -> East, pos
            | East, pos -> South, pos
            | South, pos -> West, pos
            | West, pos -> North, pos
        | 'L' ->
            match robot with
            | North, pos -> West, pos
            | East, pos -> North, pos
            | South, pos -> East, pos
            | West, pos -> South, pos
        | 'A' ->
            match robot with
            | dir, (x, y) ->
                match dir with
                | North -> dir, (x, y + 1)
                | East -> dir, (x + 1, y)
                | South -> dir, (x, y - 1)
                | West -> dir, (x - 1, y)
        | _ -> robot

    instructions |> Seq.fold moveOne robot

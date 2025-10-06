module Accumulate

// https://blog.ploeh.dk/2015/12/22/tail-recurse/
let accumulate (func: 'a -> 'b) (input: 'a list) : 'b list =
    let rec accumulateImpl acc =
        function
        | [] -> acc
        | first :: tail -> accumulateImpl (func first :: acc) tail

    accumulateImpl [] input |> List.rev

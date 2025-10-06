module Accumulate

// https://blog.ploeh.dk/2015/12/22/tail-recurse/
let accumulate (func: 'a -> 'b) (input: 'a list) : 'b list =
    let cons x xs = x :: xs

    let accumulateImpl = List.fold (fun acc first -> cons (func first) >> acc) id

    accumulateImpl input []

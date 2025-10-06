module QueenAttack

let create (row, col) =
    0 <= row && row < 8 && 0 <= col && col < 8

let canAttack (row1, col1) (row2, col2) =
    row1 = row2
    || col1 = col2
    || row1 + col1 = row2 + col2
    || row1 - col1 = row2 - col2

module BankAccount

type Account = { mutable Balance: decimal option }

let mkBankAccount () = { Balance = None }

let openAccount account =
    lock account (fun () -> account.Balance <- Some 0m)
    account

let closeAccount account =
    lock account (fun () -> account.Balance <- None)
    account

let getBalance = _.Balance

let updateBalance change account =
    lock account (fun () -> account.Balance <- account.Balance |> Option.map ((+) change))

    account

module BankAccount

type Account =
    { mutable State: bool
      mutable Balance: decimal }

let mkBankAccount () = { State = false; Balance = 0m }

let openAccount account =
    lock account (fun () -> account.State <- true)
    account

let closeAccount account =
    lock account (fun () -> account.State <- false)
    account

let getBalance account =
    match account with
    | { State = true } -> Some account.Balance
    | { State = false } -> None

let updateBalance change account =
    lock account (fun () ->
        match account with
        | { State = true } -> account.Balance <- account.Balance + change
        | { State = false } -> failwith "closed")

    account

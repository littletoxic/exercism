module BinarySearchTree

type Node<'a> =
    { Data: 'a
      Left: Node<'a> option
      Right: Node<'a> option }

let left node = node.Left

let right node = node.Right

let data node = node.Data

let createNode item =
    { Data = item
      Left = None
      Right = None }
    |> Some

let create items =

    let rec insert node item =
        match node with
        | None -> createNode item
        | Some n when item <= n.Data -> { n with Left = insert n.Left item } |> Some
        | Some n -> { n with Right = insert n.Right item } |> Some

    items |> List.fold insert None |> Option.get

(*
let sortedData node =

    let rec loop =
        function
        | None -> []
        | Some node -> loop node.Left @ [ node.Data ] @ loop node.Right

    loop (Some node)
*)

let sortedData node =

    let rec inorder =
        function
        | Some node ->
            seq {
                yield! inorder node.Left
                yield node.Data
                yield! inorder node.Right
            }
        | None -> Seq.empty


    inorder (Some node) |> Seq.toList

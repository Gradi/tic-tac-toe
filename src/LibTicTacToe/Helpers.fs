module LibTicTacToe.Helpers

let isAllEqualBy selector list =
    match list with
    | []
    | [ _ ] -> true
    | list ->
        let head = list |> (List.head >> selector)

        list
        |> List.forall (fun item -> (selector item) = head)

let tryMaxBy selector seq =
    let folder state element =
        match state with
        | None -> Some (element, selector element)
        | Some (prevElement, prevValue) ->
            let value = selector element
            if value > prevValue then Some (element, value)
            else Some (prevElement, prevValue)

    seq
    |> Seq.fold folder None
    |> Option.map fst


module SeqHelpers =

    let findOrFold folder (left, right) (seq: 'T seq) =
        use e = seq.GetEnumerator ()

        let mutable left = left
        let mutable right = right

        while (Option.isNone left && e.MoveNext ()) do
            let current = e.Current
            let cleft, cright = folder (left, right) current

            left <- cleft
            right <- cright

        match left, right with
        | Some left, _ -> left
        | None, right -> right

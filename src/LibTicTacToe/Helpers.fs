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

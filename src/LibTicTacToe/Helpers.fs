module LibTicTacToe.Helpers

let isAllEqualBy selector list =
    match list with
    | []
    | [ _ ] -> true
    | list ->
        let head = list |> (List.head >> selector)

        list
        |> List.forall (fun item -> (selector item) = head)

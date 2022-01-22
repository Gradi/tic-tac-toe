module LibTicTacToe.BotLogic

open System.Threading

open LibTicTacToe.Domain
open LibTicTacToe.Logic
open LibTicTacToe.Helpers

let gridToSearchTree firstMove grid =

    let rec _gridToSTree moveAs grid state =
        match state with
        | Draw
        | XWon _
        | OWon _ -> { Grid = grid; State = state; MoveAs = moveAs; Steps = Seq.empty }

        | Begining
        | Playable ->
            let steps =
                enumerateEmptyCells grid
                |> Seq.map (fun cell ->
                    let grid = withMoveAt cell moveAs grid
                    let state = getGridState grid

                    { Row = cell.Row; Col = cell.Col; Tree = _gridToSTree (oppositeMove moveAs) grid state })

            { Grid = grid; State = state; MoveAs = moveAs; Steps = steps }

    _gridToSTree firstMove grid (getGridState grid)

let getBestMove (cancellationToken: CancellationToken) limit meMoveAs grid =

    let rec alphabeta limit alpha beta step =

        match step.Tree.State, isLimitReached limit, cancellationToken.IsCancellationRequested with
        | Draw, _, _ -> 2
        | XWon _, _, _ when meMoveAs = MoveAs.X -> 3
        | OWon _, _, _ when meMoveAs = MoveAs.O -> 3
        | XWon _, _, _ | OWon _, _, _ -> 0
        | _, true, _
        | _, _, true -> 1

        | Begining, _, _
        | Playable, _, _ ->
            let limit = nextLimit limit

            if step.Tree.MoveAs = meMoveAs then

                step.Tree.Steps
                |> Seq.scan (fun (alpha, prevScore) step ->
                    let score = alphabeta limit alpha beta step
                    let score = max score prevScore
                    (max alpha score, score)) (alpha, System.Int32.MinValue)
                |> SeqHelpers.findOrFold (fun (_, prevScore) (_, score) ->
                    if score >= beta then (Some score, score)
                    else (None, max prevScore score)) (None, System.Int32.MinValue)

            else

                step.Tree.Steps
                |> Seq.scan (fun (beta, prevScore) step ->
                    let score = alphabeta limit alpha beta step
                    let score = min score prevScore
                    (min beta score, score)) (beta, System.Int32.MaxValue)
                |> SeqHelpers.findOrFold (fun (_, prevScore) (_, score) ->
                    if score <= alpha then (Some score, score)
                    else (None, min score prevScore)) (None, System.Int32.MaxValue)

    let tree = gridToSearchTree meMoveAs grid

    let bestMove =
        tree.Steps
        |> Seq.map (fun step -> async { return (step, alphabeta limit System.Int32.MinValue System.Int32.MaxValue step) })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.ofArray
        |> tryMaxBy snd
        |> Option.map fst
        |> Option.map (fun step -> { Row = step.Row; Col = step.Col; GridAfter = step.Tree.Grid; StateAfter = step.Tree.State })

    { Grid = tree.Grid;  State = tree.State; Move = bestMove }

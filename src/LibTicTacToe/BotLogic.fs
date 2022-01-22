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

    let alpha0 = System.Int32.MinValue
    let beta0  = System.Int32.MaxValue
    let accum0 = 0

    let willEnemyWin (step: Step) =
        let grid = withCellAt step.Row step.Col (moveAsToCellType (oppositeMove meMoveAs)) step.Tree.Grid
        match getGridState grid with
        | Begining
        | Playable
        | Draw -> false
        | XWon _ when meMoveAs = MoveAs.O -> true
        | OWon _ when meMoveAs = MoveAs.X -> true
        | XWon _
        | OWon _ -> false

    let rec alphabeta limit alpha beta accum step =

        match step.Tree.State, isLimitReached limit, cancellationToken.IsCancellationRequested with
        | XWon _, _, _ when meMoveAs = MoveAs.X -> accum + 3
        | OWon _, _, _ when meMoveAs = MoveAs.O -> accum + 3
        | Draw, _, _ -> accum + 2
        | XWon _, _, _ | OWon _, _, _ -> alpha0

        | _, true, _
        | _, _, true -> accum + 1

        | Begining, _, _
        | Playable, _, _ when willEnemyWin step -> accum + 4

        | Begining, _, _
        | Playable, _, _ ->
            let limit = nextLimit limit
            let accum = accum - 1

            if step.Tree.MoveAs = meMoveAs then

                let folder (alpha, score) step =
                    let score = max score (alphabeta limit alpha beta accum step)
                    (max alpha score, score)

                let findOrFold (_, prevScore) (_, score) =
                    if score >= beta then (Some score, score)
                    else (None, max prevScore score)

                step.Tree.Steps
                |> Seq.scan folder (alpha, alpha0)
                |> SeqHelpers.findOrFold findOrFold (None, alpha0)

            else

                let folder (beta, score) step =
                    let score = min score (alphabeta limit alpha beta accum step)
                    (min score beta, score)

                let findOrFold (_, prevScore) (_, score) =
                    if score <= alpha then (Some score, score)
                    else (None, min prevScore score)

                step.Tree.Steps
                |> Seq.scan folder (beta, beta0)
                |> SeqHelpers.findOrFold findOrFold (None, beta0)

    let tree = gridToSearchTree meMoveAs grid

    let bestMove =
        tree.Steps
        |> Seq.map (fun step -> async { return (step, alphabeta limit alpha0 beta0 accum0 step) })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.ofArray
        |> tryMaxBy snd
        |> Option.map fst
        |> Option.map (fun step -> { Row = step.Row; Col = step.Col; GridAfter = step.Tree.Grid; StateAfter = step.Tree.State })

    { Grid = tree.Grid;  State = tree.State; Move = bestMove }

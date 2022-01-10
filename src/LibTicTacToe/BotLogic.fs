module LibTicTacToe.BotLogic

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

let getBestMove meMoveAs grid =

    let rec minimax step =
        match step.Tree.State with
        | Draw -> 1
        | XWon _ when meMoveAs = MoveAs.X -> 2
        | OWon _ when meMoveAs = MoveAs.O -> 2
        | XWon _ | OWon _ -> 0

        | Begining
        | Playable ->
            let childSteps =
                step.Tree.Steps
                |> Seq.map minimax

            if step.Tree.MoveAs = meMoveAs then Seq.max childSteps
            else Seq.min childSteps

    let tree = gridToSearchTree meMoveAs grid

    let bestMove =
        tree.Steps
        |> Seq.map (fun step -> (step, minimax step))
        |> tryMaxBy snd
        |> Option.map fst
        |> Option.map (fun step -> { Row = step.Row; Col = step.Col; GridAfter = step.Tree.Grid; StateAfter = step.Tree.State })

    { Grid = tree.Grid;  State = tree.State; Move = bestMove }

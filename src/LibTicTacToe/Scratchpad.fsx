#load "Domain.fs"
#load "Helpers.fs"
#load "Logic.fs"

open LibTicTacToe.Domain
open LibTicTacToe.Logic

type MoveAs =
    | X
    | O

type DecisitionTree =
    { Grid: Grid
      State: GridState
      MoveAs: MoveAs
      Steps: Step seq }
and Step =
    { Row: int
      Col: int
      Tree: DecisitionTree }

type BestMove =
    { Row: int
      Col: int
      GridAfter: Grid
      StateAfter: GridState }

type MoveSummary =
    { Grid: Grid
      State: GridState
      Move: BestMove option }

let moveToCellType moveAs =
    match moveAs with
    | MoveAs.X -> CellType.X
    | MoveAs.O -> CellType.O

let withMoveAt (cell: Cell) moveAs grid =
    grid |> withCellAt cell.Row cell.Col (moveToCellType moveAs)

let oppositeMove move =
    match move with
    | MoveAs.X -> MoveAs.O
    | MoveAs.O -> MoveAs.X

let gridToDTree moveAs grid =

    let rec _gridToDTree moveAs grid state =
        let tree = { Grid = grid; State = state; MoveAs = moveAs; Steps = Seq.empty }

        match state with
        | Draw
        | XWon _
        | OWon _ -> tree

        | Begining
        | Playable ->
            let moves =
                enumerateEmptyCells grid
                |> Seq.map (fun cell ->
                    let nextGrid = withMoveAt cell moveAs grid
                    let nextState = getGridState nextGrid

                    { Row = cell.Row; Col = cell.Col; Tree = _gridToDTree (oppositeMove moveAs) nextGrid nextState })

            { tree with Steps = moves }

    _gridToDTree moveAs grid (getGridState grid)

let tryMinBy selector seq =
    let folder state elem =
        match state with
        | None -> Some (elem, selector elem)
        | Some (prevElem, prevValue) ->
            let value = selector elem
            if value < prevValue then Some (elem, value)
            else Some (prevElem, prevValue)

    seq
    |> Seq.fold folder None
    |> Option.map fst

let countStepsToWin moveAs step =
    let rec _countStepsToWin step =
        match step.Tree.State with
        | Draw -> Some 1

        | XWon _ when moveAs = MoveAs.X -> Some 1
        | OWon _ when moveAs = MoveAs.O -> Some 1

        | XWon _
        | OWon _ -> None

        | Begining
        | Playable ->
            let minimum =
                step.Tree.Steps
                |> Seq.map _countStepsToWin
                |> Seq.choose id
                |> tryMinBy id

            minimum
            |> Option.map (fun m -> m + 1)

    _countStepsToWin step

let stepToBestMove (step: Step): BestMove =
    { Row = step.Row; Col = step.Col; GridAfter = step.Tree.Grid; StateAfter = step.Tree.State }

let getBestMove moveAs grid =
    let tree = gridToDTree moveAs grid

    let moveSummary = { Grid = grid; State = tree.State; Move = None }

    let steps = tree.Steps |> Seq.cache

    let minimumDistanceToWin =
        steps
        |> Seq.map (fun step ->
            countStepsToWin moveAs step
            |> Option.map (fun dis -> (step, dis)))
        |> Seq.choose id
        |> tryMinBy snd
        |> Option.map fst

    match minimumDistanceToWin with
    | None -> { moveSummary with Move = Seq.tryHead steps |> Option.map stepToBestMove}
    | Some step -> { moveSummary with Move = Some <| stepToBestMove step }

let gridToString grid =
    let mapper cell =
        match cell.Type with
        | CellType.Empty -> " "
        | CellType.X -> "X"
        | CellType.O -> "O"

    seq {
        for r = 0 to grid.Rows - 1 do
            yield seq {
                for c = 0 to grid.Cols - 1 do
                    yield mapper (cellAt r c grid)
            }
    }
    |> Seq.map (String.concat " | ")
    |> String.concat "\n"

let rec tryGrid moveAs grid =
    printfn $"{gridToString grid}"
    printfn " TO"
    let move = getBestMove moveAs grid
    match move.Move with
    | None ->
        printfn $"DONE: %A{move.State}"
    | Some move ->
        tryGrid (oppositeMove moveAs) move.GridAfter


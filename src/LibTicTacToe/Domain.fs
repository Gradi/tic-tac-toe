module LibTicTacToe.Domain

type CellType =
    | Empty
    | X
    | O

type Cell = { Row: int; Col: int; Type: CellType }

type Grid =
    { Rows: int
      Cols: int
      WinLength: int
      Cells: Cell array }

type WinLine = Cell list

type GridState =
    | Begining
    | Playable
    | Draw
    | XWon of WinLine
    | OWon of WinLine

type MoveAs =
    | X
    | O

type SearchTree =
    { Grid: Grid
      State: GridState
      MoveAs: MoveAs
      Steps: Step seq }
and Step =
    { Row: int
      Col: int
      Tree: SearchTree }

type BestMove =
    { Row: int
      Col: int
      GridAfter: Grid
      StateAfter: GridState }

type MoveSummary =
    { Grid: Grid
      State: GridState
      Move: BestMove option }


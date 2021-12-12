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

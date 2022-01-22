namespace TicTacToe.Models

open LibTicTacToe.Domain

type InitialGridModel =
    { Rows: int
      Cols: int
      WinLength: int
      MoveAs: MoveAs }


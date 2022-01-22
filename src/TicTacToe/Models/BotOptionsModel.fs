namespace TicTacToe.Models

open LibTicTacToe.Domain

type BotOptionsModel =
    { OLimit: Limiter
      XLimit: Limiter }


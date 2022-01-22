# Tic Tac Toe

This is naive, unoptimized, probably bugged library that contains types and
functions to work with tic-tac-toe game.

It also contains simple GUI to test and play around.

## Running

- Install .NET 6
- Issue command

```
dotnet run --project .\src\TicTacToe\TicTacToe.fsproj -c Release
```

You can limit maximum depth (or time) of tree search in **File -> Bot
options**. But anyway, for any grid larger than 3x3 it takes forever to make a
turn.

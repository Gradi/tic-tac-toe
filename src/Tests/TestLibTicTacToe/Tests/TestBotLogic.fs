module TestLibTicTacToe.Tests.TestBotLogic

open FsUnit
open NUnit.Framework

open LibTicTacToe.Domain
open LibTicTacToe.Logic
open LibTicTacToe.BotLogic

let rec runGrid moveAs grid =
    let move = getBestMove moveAs grid

    match move.Move with
    | None -> move.Grid
    | Some move -> runGrid (oppositeMove moveAs) move.GridAfter

module ``getBestMove`` =

    [<Test>]
    let ``For empty 3x3 grid match always results in Draw`` () =
        newGrid (3, 3) 3
        |> runGrid MoveAs.X
        |> getGridState
        |> should equal Draw

        newGrid (3, 3) 3
        |> runGrid MoveAs.O
        |> getGridState
        |> should equal Draw

    [<Test>]
    let ``For 3x3 grid with single occupied cell results in Draw`` () =
        for r = 0 to 2 do
            for c = 0 to 2 do
                newGrid (3, 3) 3
                |> withCellAt r c CellType.X
                |> runGrid MoveAs.O
                |> getGridState
                |> should equal Draw




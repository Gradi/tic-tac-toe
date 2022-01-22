module TestLibTicTacToe.Tests.TestBotLogic

open FsUnit
open NUnit.Framework
open System.Threading

open LibTicTacToe.Domain
open LibTicTacToe.Logic
open LibTicTacToe.BotLogic

let rec runGrid limit moveAs grid =
    let move = getBestMove CancellationToken.None limit moveAs grid

    match move.Move with
    | None -> move.Grid
    | Some move -> runGrid limit (oppositeMove moveAs) move.GridAfter

module ``getBestMove`` =

    [<Test>]
    let ``For empty 3x3 grid match always results in Draw`` () =
        newGrid (3, 3) 3
        |> runGrid limitUnlimited MoveAs.X
        |> getGridState
        |> should equal Draw

        newGrid (3, 3) 3
        |> runGrid limitUnlimited MoveAs.O
        |> getGridState
        |> should equal Draw

    [<Test>]
    let ``For 3x3 grid with single occupied cell results in Draw`` () =
        for r = 0 to 2 do
            for c = 0 to 2 do
                newGrid (3, 3) 3
                |> withCellAt r c CellType.X
                |> runGrid limitUnlimited MoveAs.O
                |> getGridState
                |> should equal Draw

    [<Test>]
    let ``Grid 5x5 with limit should end in reasonable time`` () =
        newGrid (5, 5) 5
        |> runGrid (limitByDepth 1) MoveAs.X
        |> getGridState
        |> should equal Draw




namespace BenchmarkLibTicTacToe

open BenchmarkDotNet.Attributes

open LibTicTacToe.Domain
open LibTicTacToe.Logic
open LibTicTacToe.BotLogic

type BenchmarkBotLogic () =

    let grid = newGrid (3, 3) 3

    [<Benchmark>]
    member _.GetBestMove () = getBestMove MoveAs.X grid

    [<Benchmark>]
    member _.RunGridTillDraw () =
        let mutable currentGrid = grid
        let mutable moveAs = MoveAs.X
        let mutable move = getBestMove moveAs grid

        if move.State = Draw then
            failwith "Grid is at Draw state right at the begining."

        while Option.isSome move.Move do
            currentGrid <- move.Move.Value.GridAfter
            moveAs <- oppositeMove moveAs
            move <- getBestMove moveAs currentGrid

        if move.State <> Draw then
            failwith "Expected final state to be Draw."

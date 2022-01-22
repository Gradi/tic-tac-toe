namespace TicTacToe.ViewModels

open ReactiveUI
open System.Threading
open System.Threading.Tasks

open LibTicTacToe.Domain
open LibTicTacToe.Logic
open LibTicTacToe.BotLogic
open TicTacToe.Models

type MainWindowViewModel() as this =
    inherit ViewModelBase()

    let mutable initialGridModel    = { Rows = 3; Cols = 3; WinLength = 3; MoveAs = MoveAs.X }
    let mutable botOptions          = { OLimit = limitByDepth 10; XLimit = limitByDepth 10 }

    let mutable grid                = newGrid (initialGridModel.Rows, initialGridModel.Cols) initialGridModel.WinLength
    let mutable gridState           = getGridState grid
    let mutable currentMoveAs       = initialGridModel.MoveAs

    let mutable isAutomoveEnabled   = false

    let mutable isBusy              = false

    let mutable cancellationTokenSource = new CancellationTokenSource ()

    let isGridPlayable () =
        match this.GridState with
        | Begining
        | Playable -> true
        | Draw
        | XWon _
        | OWon _ -> false

    let isAutomoveAvailable () = isAutomoveEnabled && isGridPlayable ()

    let busy (f: unit -> Task<unit>) =
        task {
            if not isBusy then
                isBusy <- true
                this.RaisePropertyChanged (nameof this.Busy)

                let! _ = f ()

                isBusy <- false
                this.RaisePropertyChanged (nameof this.Busy)
        } |> ignore

    let switchMove () = currentMoveAs <- oppositeMove currentMoveAs

    let raisePropsChanged () =
        this.RaisePropertyChanged (nameof this.Grid)
        this.RaisePropertyChanged (nameof this.GridState)
        this.RaisePropertyChanged (nameof this.CurrentMoveAs)

    let getCurrentLimiter () =
        match currentMoveAs with
        | MoveAs.X -> botOptions.XLimit
        | MoveAs.O -> botOptions.OLimit

    let innerDoBotMove () = task {
        if isGridPlayable () then
            try
                use tokenSource = new CancellationTokenSource ()
                use _ = Interlocked.Exchange(&cancellationTokenSource, tokenSource)

                let! move = Task.Run ((fun () -> getBestMove tokenSource.Token (getCurrentLimiter ()) currentMoveAs grid), tokenSource.Token)

                match move.Move with
                | None -> ()
                | Some move ->
                    grid <- move.GridAfter
                    gridState <- move.StateAfter
                    switchMove ()
                    raisePropsChanged ()
            with
            | :? TaskCanceledException -> ()
    }

    member _.InitialGridModel
        with get () = initialGridModel
        and set value = this.RaiseAndSetIfChanged(&initialGridModel, value) |> ignore

    member _.BotOptions
        with get () = botOptions
        and set value = this.RaiseAndSetIfChanged(&botOptions, value) |> ignore

    member _.Grid = grid

    member _.GridState = gridState

    member _.CurrentMoveAs = currentMoveAs

    member _.IsAutomoveEnabled
        with get () = isAutomoveEnabled
        and set value = this.RaiseAndSetIfChanged(&isAutomoveEnabled, value) |> ignore

    member _.Busy = isBusy

    member _.DoTheMove (cell: Cell) =
        (fun () -> task {
            if isGridPlayable () && cell.Type = Empty then
                let nextGrid = withMoveAt cell this.CurrentMoveAs this.Grid
                let! state = Task.Run (fun () -> getGridState nextGrid)

                grid <- nextGrid
                gridState <- state
                switchMove ()
                raisePropsChanged ()
        })
        |> busy

    member _.DoBotMove () =
        (fun () -> task {
            let! _ = innerDoBotMove ()

            while isAutomoveAvailable () do
                let! _ = Task.Delay (System.TimeSpan.FromSeconds 1)
                let! _ = innerDoBotMove ()
                ()
        })
        |> busy

    member _.Reset () =
        (fun () -> task {
            grid <- newGrid (initialGridModel.Rows, initialGridModel.Cols) initialGridModel.WinLength
            gridState <- getGridState grid
            currentMoveAs <- initialGridModel.MoveAs

            raisePropsChanged ()
        })
        |> busy

    member _.StopAutomove () =
        this.IsAutomoveEnabled <- false
        try
            cancellationTokenSource.Cancel ()
        with
        | :? System.ObjectDisposedException -> ()

        System.Diagnostics.Debug.WriteLine "StopAutomove"


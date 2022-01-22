namespace TicTacToe.ViewModels

open ReactiveUI

open LibTicTacToe.Domain
open TicTacToe.Models

type NewGridWindowViewModel() as this =
    inherit ViewModelBase()

    let mutable rows = 3
    let mutable cols = 3
    let mutable winLength = 3
    let mutable moveAsIndex = 0

    let getMoveAs () =
        match moveAsIndex with
        | 0 -> MoveAs.X
        | 1 -> MoveAs.O
        | v -> failwithf $"Unknown representation of MoveAs: \"%d{v}\". Expected \"0\" or \"1\"."

    member _.Rows
        with get () = rows
        and set value =
            this.RaiseAndSetIfChanged(&rows, value) |> ignore
            this.RaisePropertyChanged "IsConfigurationValid"

    member _.Cols
        with get () = cols
        and set value =
            this.RaiseAndSetIfChanged(&cols, value) |> ignore
            this.RaisePropertyChanged "IsConfigurationValid"

    member _.WinLength
        with get () = winLength
        and set value =
            this.RaiseAndSetIfChanged(&winLength, value) |> ignore
            this.RaisePropertyChanged "IsConfigurationValid"

    member _.MoveAsIndex
        with get () = moveAsIndex
        and set value = this.RaiseAndSetIfChanged(&moveAsIndex, value) |> ignore

    member _.IsConfigurationValid =
        this.Rows > 0 && this.Cols > 0 && this.WinLength > 0

    member _.GetInitialModel () = { Rows = this.Rows; Cols = this.Cols; WinLength = this.WinLength; MoveAs = getMoveAs () }



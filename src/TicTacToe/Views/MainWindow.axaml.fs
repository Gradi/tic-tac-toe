namespace TicTacToe.Views

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Markup.Xaml

open TicTacToe.Controls
open TicTacToe.Models
open TicTacToe.ViewModels

type MainWindow () as this =
    inherit Window ()

    let thisModel () = (this.DataContext :?> MainWindowViewModel)

    do this.InitializeComponent()

    member private this.InitializeComponent() =
#if DEBUG
        this.AttachDevTools()
#endif
        AvaloniaXamlLoader.Load(this)

    member private _.MenuItem_NewGrid_Click (sender: System.Object, args: RoutedEventArgs) =
        task {
            let newGridWindow = NewGridWindow ()
            let! result = newGridWindow.ShowDialog<System.Object>(this)

            match result with
            | :? InitialGridModel as m ->
                thisModel().InitialGridModel <- m
                thisModel().Reset ()
            | _ -> ()
        }
        |> ignore

    member private _.MenuItem_BotOptions_Click (sender: System.Object, args: RoutedEventArgs) =
        task {
            let botOptionsWindow = BotOptionsWindow ()
            (botOptionsWindow.DataContext :?> BotOptionsWindowViewModel).PopulateFromBotOptionsModel (thisModel().BotOptions)

            let! result = botOptionsWindow.ShowDialog<System.Object> this

            match result with
            | :? BotOptionsModel as m ->
                thisModel().BotOptions <- m
            | _ -> ()
        }
        |> ignore

    member private _.GridControl_CellClicked (sender: System.Object, args: CellClickedRoutedEventArgs) =
        thisModel().DoTheMove args.Cell

    override _.OnKeyUp (e: KeyEventArgs) =
        match e.Key with
        | Key.Escape ->
            thisModel().StopAutomove()
            e.Handled <- true
        | _ -> base.OnKeyUp e

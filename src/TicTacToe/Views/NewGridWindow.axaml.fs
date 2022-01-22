namespace TicTacToe.Views

open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Markup.Xaml

open TicTacToe.ViewModels

type public NewGridWindow () as this =
    inherit Window ()

    do this.InitializeComponent()

    member private _.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)
        this.DataContext <- NewGridWindowViewModel ()

    member private _.Button_Ok_Click (sender: System.Object, args: RoutedEventArgs) =
        let model = (this.DataContext :?> NewGridWindowViewModel).GetInitialModel ()
        this.Close model


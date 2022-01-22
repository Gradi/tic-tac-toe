namespace TicTacToe.Views

open Avalonia.Controls
open Avalonia.Markup.Xaml
open Avalonia.Interactivity
open TicTacToe.ViewModels

type BotOptionsWindow () as this =
    inherit Window ()

    do this.InitializeComponent ()

    member private _.InitializeComponent () =
        AvaloniaXamlLoader.Load this
        this.DataContext <- BotOptionsWindowViewModel ()

    member private _.Button_OK_Click (sender: System.Object, args:RoutedEventArgs) =
        let model = (this.DataContext :?> BotOptionsWindowViewModel)
        this.Close (model.GetBotOptionsModel ())


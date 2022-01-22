namespace TicTacToe.Controls

open Avalonia
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Media

open LibTicTacToe.Domain
open LibTicTacToe.Logic

type CellClickedRoutedEventArgs (cell: Cell) =
    inherit RoutedEventArgs ()

    member _.Cell = cell

type TicTacToeGridControl () as this =
    inherit Avalonia.Controls.Control ()

    let blackPen     = Pen (SolidColorBrush Colors.Black, 2)
    let redPen       = Pen (SolidColorBrush Colors.Red, 2)
    let bluePen      = Pen (SolidColorBrush Colors.Blue, 2)
    let grayPen      = Pen (SolidColorBrush Colors.LightGray, 2)
    let greenBoldPen = Pen (SolidColorBrush Colors.Green, 6)

    let emptyBrush = SolidColorBrush (Colors.White, Opacity = 0.0)
    let blackBrush = SolidColorBrush Colors.Black

    let cellClickedEvent = Event<CellClickedRoutedEventArgs> ()

    let mutable pointerPoint = Point ()

    let cellWidth () = this.Bounds.Width / (float this.Grid.Cols)
    let cellHeight () = this.Bounds.Height / (float this.Grid.Rows)

    let cellToPen (cell: Cell) =
        match cell.Type with
        | CellType.Empty -> blackPen
        | CellType.X -> bluePen
        | CellType.O -> redPen

    let cellToPoint (cell: Cell) =
        let x = (float cell.Col) * cellWidth ()
        let y = (float cell.Row) * cellHeight ()

        Point (x, y)

    let tryGetSelectedCell () =
        let w = cellWidth ()
        let h = cellHeight ()

        this.Grid.Cells
        |> Array.tryFind (fun cell ->
            let point = cellToPoint cell

            pointerPoint.X >= point.X && pointerPoint.X < (point.X + w) &&
            pointerPoint.Y >= point.Y && pointerPoint.Y < (point.Y + h)
            )

    let drawCell (context: DrawingContext) pen (cell: Cell) =
        let w = cellWidth ()
        let h = cellHeight ()
        let margin = 15.0
        let point = cellToPoint cell

        match cell.Type with
        | CellType.Empty -> ()
        | CellType.X ->
            context.DrawLine (pen, Point (point.X + margin, point.Y + margin), Point (point.X + w - margin, point.Y + h - margin))
            context.DrawLine (pen, Point (point.X + w - margin, point.Y + margin), Point (point.X + margin, point.Y + h - margin))
        | CellType.O ->
            let radius = (min w h) / 2.0 - margin
            context.DrawEllipse (emptyBrush, pen, Point (point.X + w / 2.0, point.Y + h / 2.0), radius, radius)

    let drawGrid (context: DrawingContext) =
        let w = cellWidth ()
        let h = cellHeight ()
        let grid = this.Grid

        for c = 1 to grid.Cols - 1 do
            let c = float c
            context.DrawLine (blackPen, Point (c * w, 0), Point ( c * w, this.Bounds.Height))

        for r = 1 to grid.Rows - 1 do
            let r = float r
            context.DrawLine (blackPen, Point(0, r * h), Point (this.Bounds.Width, r * h))

        for cell in grid.Cells do
            drawCell context (cellToPen cell) cell

    let drawState (context: DrawingContext) =
        match this.GridState with
        | Begining
        | Playable
        | Draw -> ()

        | XWon line
        | OWon line ->
            for cell in line do
                drawCell context greenBoldPen cell

    let drawCurrentSelectedCell (context: DrawingContext) =
        match tryGetSelectedCell () with
        | None -> ()
        | Some cell when cell.Type <> CellType.Empty -> ()
        | Some cell -> drawCell context grayPen { cell with Type = moveAsToCellType this.CurrentMoveAs }

    let drawSelectedCellIndexes (context: DrawingContext) =
        match tryGetSelectedCell () with
        | None -> ()
        | Some cell ->
            let fmtText = FormattedText ()
            fmtText.Text <- sprintf $"(R: %d{cell.Row}, C: %d{cell.Col})"
            fmtText.TextAlignment <- TextAlignment.Left
            fmtText.FontSize <- 16
            fmtText.Typeface <- Typeface.Default

            let point = cellToPoint cell
            let textCoords = Point (point.X + cellWidth () / 2.0 - fmtText.Bounds.Width / 2.0, point.Y + cellWidth () / 20.0)
            context.DrawText (blackBrush, textCoords, fmtText)



    static let GridProperty: StyledProperty<Grid> =
        AvaloniaProperty.Register<TicTacToeGridControl, Grid>("Grid", defaultValue = newGrid (3, 3) 3)

    static let GridStateProperty: StyledProperty<GridState> =
        AvaloniaProperty.Register<TicTacToeGridControl, GridState>("GridState", defaultValue = GridState.Begining)

    static let CurrentMoveAsProperty: StyledProperty<MoveAs> =
        AvaloniaProperty.Register<TicTacToeGridControl, MoveAs>("CurrentMoveAs", defaultValue = MoveAs.X)

    static let CellClickedEvent: RoutedEvent<CellClickedRoutedEventArgs> =
        RoutedEvent.Register<TicTacToeGridControl, CellClickedRoutedEventArgs>("CellClicked", RoutingStrategies.Bubble)

    member _.Grid
        with get () = this.GetValue<Grid>(GridProperty)
        and set value = this.SetValue(GridProperty, value) |> ignore

    member _.GridState
        with get () = this.GetValue<GridState>(GridStateProperty)
        and set value = this.SetValue(GridStateProperty, value) |> ignore

    member _.CurrentMoveAs
        with get () = this.GetValue<MoveAs>(CurrentMoveAsProperty)
        and set value = this.SetValue(CurrentMoveAsProperty, value) |> ignore

    [<CLIEvent>]
    member _.CellClicked = cellClickedEvent.Publish

    override _.Render(context: DrawingContext) =
        context.FillRectangle (SolidColorBrush Colors.White, this.Bounds)
        drawGrid context
        drawState context
        drawCurrentSelectedCell context
        drawSelectedCellIndexes context

    override _.OnPointerMoved(e: PointerEventArgs) =
        let position = e.GetPosition this

        pointerPoint <- Point (position.X, position.Y)
        this.InvalidateVisual ()

        base.OnPointerMoved e

    override _.OnPointerPressed (e: PointerPressedEventArgs) =
        match tryGetSelectedCell () with
        | None -> ()
        | Some cell -> cellClickedEvent.Trigger (CellClickedRoutedEventArgs cell)

        base.OnPointerPressed e

    override _.OnPropertyChanged<'T> (change: AvaloniaPropertyChangedEventArgs<'T>) =
        this.InvalidateVisual ()
        base.OnPropertyChanged change

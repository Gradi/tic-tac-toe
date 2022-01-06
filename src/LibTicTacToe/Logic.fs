module LibTicTacToe.Logic

open Domain

let newGrid (rows, cols) winLength =
    match (rows, cols, winLength) with
    | rows, cols, _ when rows <= 0 || cols <= 0 -> failwith "Rows and cols should be positive number."
    | _, _, winLength when winLength <= 0 -> failwith "Win length should be positive number."
    | rows, cols, winLength when winLength > rows && winLength > cols ->
        failwith "Win length is greater than row or column number. You can't win."

    | rows, cols, winLength ->
        let cells =
            Array.init rows (fun row -> Array.init cols (fun col -> { Row = row; Col = col; Type = Empty }))
            |> Array.concat

        { Rows = rows
          Cols = cols
          WinLength = winLength
          Cells = cells }

let newGridFromCells winLength types =
    match (types, winLength) with
    | _, winLength when winLength <= 0 -> failwith "Win length should be positive number."
    | [], _ -> failwith "Can't create grid from empty list."
    | types, _ when not (Helpers.isAllEqualBy List.length types) ->
        failwith "List of list must be rectangular (eg. It's rows must have same number of columns)."

    | types, winLength ->
        let mapper (row, columns) =
            List.indexed columns
            |> List.map (fun (col, type') -> { Row = row; Col = col; Type = type' })

        let cells = List.indexed types |> List.map mapper

        let rows, cols =
            (List.length cells, (List.head >> List.length) cells)

        { Rows = rows
          Cols = cols
          WinLength = winLength
          Cells = (List.concat >> Array.ofList) cells }

let cellAt row col grid =
    match (row, col) with
    | row, col when
        row < 0
        || row >= grid.Rows
        || col < 0
        || col >= grid.Cols
        ->
        failwithf $"Indexes are out of bounds. Indexes: (%d{row}, %d{col}). Grid size: (%d{grid.Rows}, %d{grid.Cols})."
    | row, col ->
        let index = row * grid.Cols + col
        Array.item index grid.Cells

let isAllEmpty grid =
    grid.Cells
    |> Array.forall (fun cell -> cell.Type = Empty)

let isContainsEmpty grid =
    Option.isSome
    <| Array.tryFind
        (fun cell ->
            match cell.Type with
            | Empty -> true
            | X
            | O -> false)
        grid.Cells

let enumerateCells grid =
    let rows, cols = grid.Rows, grid.Cols
    let maxDiagonal = max rows cols

    let isValidCoord (row, col) =
        row >= 0 && row < rows && col >= 0 && col < cols

    let filterCoords seq =
        seq
        |> Seq.takeWhile isValidCoord
        |> Seq.map (fun (row, col) -> cellAt row col grid)

    seq {
        // Horizonal
        for row = 0 to rows - 1 do
            yield
                seq {
                    for col = 0 to cols - 1 do
                        yield cellAt row col grid
                }

        // Vertical
        for col = 0 to cols - 1 do
            yield
                seq {
                    for row = 0 to rows - 1 do
                        yield cellAt row col grid
                }

        // Left to Right, Top to Down starting from bottom left cell
        for row = rows - 1 downto 0 do
            yield
                seq {
                    for i = 0 to maxDiagonal do
                        yield (row + i, i)
                }
                |> filterCoords

        for col = 1 to cols - 1 do
            yield
                seq {
                    for i = 0 to maxDiagonal do
                        yield (i, col + i)
                }
                |> filterCoords

        //Right to Left, Top to Down starting from bottom right cell
        for row = rows - 1 downto 0 do
            yield
                seq {
                    for i = 0 to maxDiagonal do
                        yield (row + i, cols - 1 - i)
                }
                |> filterCoords

        for col = cols - 2 downto 0 do
            yield
                seq {
                    for i = 0 to maxDiagonal do
                        yield (i, col - i)
                }
                |> filterCoords
    }

let getWinLine grid : WinLine list =
    let isSameType a b = a.Type = b.Type
    let isLinesOfSameType a b = List.head a |> isSameType b
    let isWinLine a = List.length a = grid.WinLength

    let folder state cell =
        match cell with
        | { Row = _; Col = _; Type = Empty } ->
            match state with
            | _, [] -> state
            | listOfLines, buffer when isWinLine buffer -> (List.append listOfLines [ buffer ], [])
            | listOfLines, _ -> (listOfLines, [])

        | { Row = _; Col = _; Type = X | O } ->
            match state with
            | listOfLines, [] -> (listOfLines, [ cell ])

            | listOfLines, buffer when isWinLine buffer && isLinesOfSameType buffer cell ->
                let current = buffer
                let next = List.append (List.tail buffer) [ cell ]
                let newBuffer = List.tail next
                (List.append listOfLines [ current; next ], newBuffer)

            | listOfLines, buffer when isWinLine buffer -> (List.append listOfLines [ buffer ], [ cell ])

            | listOfLines, buffer when not (isLinesOfSameType buffer cell) -> (listOfLines, [ cell ])

            | listOfLines, buffer -> (listOfLines, List.append buffer [ cell ])

    let finishState state =
        match state with
        | [], [] -> None
        | listOfLines, [] -> Some listOfLines
        | listOfLines, buffer when isWinLine buffer -> Some <| List.append listOfLines [ buffer ]
        | [], _ -> None
        | listOfLines, _ -> Some listOfLines

    let mapper = Seq.fold folder ([], []) >> finishState

    enumerateCells grid
    |> Seq.map mapper
    |> Seq.choose id
    |> Seq.fold (fun state list -> List.append state list) []

let getGridState grid =
    match grid.Cells with
    | [||] -> Draw
    | _ ->
        match isAllEmpty grid with
        | true -> Begining
        | false ->
            match getWinLine grid with
            | [] when isContainsEmpty grid -> Playable
            | [] -> Draw
            | winLines when Helpers.isAllEqualBy (fun c -> c.Type) (List.concat winLines) ->
                let winLine = List.head winLines
                let cellType = (List.head winLine).Type

                match cellType with
                | Empty -> failwith "Should not happen"
                | X -> XWon winLine
                | O -> OWon winLine

            | _ -> Draw


let withCellAt row col cellType grid =
    let cell = { Row = row; Col = col; Type = cellType }
    let index = row * grid.Cols + col
    { grid with Cells = grid.Cells |> Array.updateAt index cell }

let enumerateEmptyCells grid =
    Seq.ofArray grid.Cells
    |> Seq.filter (fun c -> c.Type = Empty)

module TestLibTicTacToe.Tests.TestLogic

open NUnit.Framework
open FsUnit

open LibTicTacToe.Domain
open LibTicTacToe.Logic


module ``newGrid`` =

    [<Test>]
    let ``Fails with invalid arguments`` () =
        (fun () -> newGrid (0, 0) 0 |> ignore) |> should throw typeof<System.Exception>
        (fun () -> newGrid (0, -1) 0 |> ignore) |> should throw typeof<System.Exception>
        (fun () -> newGrid (-1, 0) 0 |> ignore) |> should throw typeof<System.Exception>
        (fun () -> newGrid (1, 1) 0 |> ignore) |> should throw typeof<System.Exception>
        (fun () -> newGrid (1, 1) -1 |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    let ``Fails when winLength is greater than both dimensions``
        ([<Range(1, 10)>] rows: int)
        ([<Range(1, 10)>] cols: int)
        ([<Range(1, 10)>] step: int)
        =
        (fun () -> newGrid (rows, cols) ((max rows cols) + step) |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    let ``Successfully creates grid with correct values``
        ([<Range(1, 10)>] rows: int)
        ([<Range(1, 10)>] cols: int)
        =
        let winLength = 1
        let grid = newGrid (rows, cols) winLength

        grid.Rows |> should equal rows
        grid.Cols |> should equal cols
        grid.WinLength |> should equal winLength

        Array.length grid.Cells
        |> should equal (rows * cols)

        Array.forall (fun cell -> cell.Type = CellType.Empty) grid.Cells
        |> should be True

        Array.pairwise grid.Cells
        |> Array.map (fun (c1, c2) -> ((c1.Row, c1.Col), (c2.Row, c2.Col)))
        |> Array.forall (fun (c1, c2) -> c1 < c2)
        |> should be True

module ``newGridFromCells`` =

    let X = CellType.X
    let O = CellType.O

    [<Test>]
    let ``Fails with invalid arguments`` () =
        (fun () ->
            newGridFromCells 0 [ [ CellType.Empty ] ]
            |> ignore)
         |> should throw typeof<System.Exception>

        (fun () ->
            newGridFromCells -1 [ [ CellType.Empty ] ]
            |> ignore)
         |> should throw typeof<System.Exception>

        (fun () -> newGridFromCells 1 [] |> ignore) |> should throw typeof<System.Exception>

        (fun () ->
            newGridFromCells 1 [ [ X; O ]; [ X ]; [ O; X ] ]
            |> ignore)
         |> should throw typeof<System.Exception>

    [<Test>]
    let ``Successfully creates grid``
        ([<Range(1, 10)>] rows: int)
        ([<Range(1, 10)>] cols: int)
        ([<Range(1, 3)>] cellType: int)
        =
        let winLength = 1
        let type' =
            match cellType with
            | 1 -> CellType.Empty
            | 2 -> CellType.X
            | 3 -> CellType.O
            | _ -> failwith "Int out of range"

        let types =
            List.init rows (fun _ -> List.init cols (fun _ -> type'))

        let grid = newGridFromCells winLength types

        grid.Rows |> should equal rows
        grid.Cols |> should equal cols
        grid.WinLength |> should equal winLength

        Array.length grid.Cells
        |> should equal (rows * cols)

        Array.forall (fun cell -> cell.Type = type') grid.Cells
        |> should be True

        Array.pairwise grid.Cells
        |> Array.map (fun (c1, c2) -> ((c1.Row, c1.Col), (c2.Row, c2.Col)))
        |> Array.forall (fun (c1, c2) -> c1 < c2)
        |> should be True

module ``cellAt`` =

    [<Test>]
    let ``Fails when indexes are out of bounds`` ([<Range(1, 10)>] rows: int) ([<Range(1, 10)>] cols: int) =
        let grid = newGrid (rows, cols) 1

        for row = -rows downto -1 do
            for col = -cols downto -1 do
                (fun () -> cellAt row col grid |> ignore) |> should throw typeof<System.Exception>

        for row = rows to rows * 2 do
            for col = cols to cols * 2 do
                (fun () -> cellAt row col grid |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    let ``Succcessfully returns cell at (row, col)`` ([<Range(1, 10)>] rows: int) ([<Range(1, 10)>] cols: int) =
        let grid = newGrid (rows, cols) 1

        for row = 0 to rows - 1 do
            for col = 0 to cols - 1 do
                let cell = cellAt row col grid

                cell.Row |> should equal row
                cell.Col |> should equal col
                cell.Type |> should equal CellType.Empty

module ``isAllEmpty`` =

    [<Test>]
    let ``Returns true for newly created grid`` ([<Range(1, 10)>] rows: int) ([<Range(1, 10)>] cols: int) =
        newGrid (rows, cols) 1
        |> isAllEmpty
        |> should be True

    [<Test>]
    let ``Returns false when grid contains some occupied cells`` ([<Range(1, 10)>] rows: int) ([<Range(1, 10)>] cols: int) =
        let grid = newGrid (rows, cols) 1

        for row = 0 to rows - 1 do
            for col = 0 to cols - 1 do
               { grid with Cells = Array.updateAt (row * cols + col) { Row = row; Col = col; Type = CellType.X } grid.Cells}
               |> isAllEmpty
               |> should be False

module ``isContainsEmpty`` =

    let X = CellType.X
    let O = CellType.O

    [<Test>]
    let ``Returns true for newly created grid`` ([<Range(1, 10)>] rows: int) ([<Range(1, 10)>] cols: int) =
        newGrid (rows, cols) 1
        |> isContainsEmpty
        |> should be True

    [<Test>]
    let ``Returns false for empty grid`` () =
        { Rows = 0; Cols = 0; WinLength = 0; Cells = [||] }
        |> isContainsEmpty
        |> should be False

    [<Test>]
    let ``Returns false for all occupied grid`` ([<Range(1, 10)>] rows: int) ([<Range(1, 10)>] cols: int) ([<Range(1, 2)>] cellType: int) =
        let type' =
            match cellType with
            | 1 -> CellType.X
            | 2 -> CellType.O
            | _ -> failwith "Int out of range."

        let cells =
            Array.init rows (fun row -> Array.init cols (fun col -> { Row = row; Col = col; Type = type' }))
            |> Array.concat

        { Rows = rows; Cols = cols; WinLength = 0; Cells = cells }
        |> isContainsEmpty
        |> should be False

    [<Test>]
    let ``Returns true for partially occupied grid`` () =
        newGridFromCells 3 [[X; O; O]; [O; CellType.Empty; X]; [X; X; X]]
        |> isContainsEmpty
        |> should be True

module ``enumerateCells returns valid seq for grids of different sizes`` =

    let (*) row col = { Row = row; Col = col; Type = CellType.Empty }

    let listOfSeqSeq seq = seq |> ((Seq.map List.ofSeq) >> List.ofSeq)

    let (!) (row, col) =
        let grid =
            match (row, col) with
            | 0, 0 -> { Rows = 0; Cols = 0; WinLength = 0; Cells = [||] }
            | row, col -> newGrid (row, col) 1

        grid
        |> enumerateCells
        |> listOfSeqSeq

    [<Test>]
    let ``0x0`` () =
        ! (0, 0)
        |> should be FsUnit.TopLevelOperators.Empty

    [<Test>]
    let ``1x1`` () =
        ! (1, 1)
        |> should equal [
            [ 0 * 0 ]

            [ 0 * 0 ]

            [ 0 * 0 ]

            [ 0 * 0 ]
        ]

    [<Test>]
    let ``1x2`` () =
        ! (1, 2)
        |> should equal [
            [ 0 * 0; 0 * 1 ]

            [ 0 * 0 ]
            [ 0 * 1 ]

            [ 0 * 0 ]
            [ 0 * 1 ]

            [ 0 * 1 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``1x3`` () =
        ! (1, 3)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2 ]

            [ 0 * 0 ]
            [ 0 * 1 ]
            [ 0 * 2 ]

            [ 0 * 0 ]
            [ 0 * 1 ]
            [ 0 * 2 ]

            [ 0 * 2 ]
            [ 0 * 1 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``1x4`` () =
        ! (1, 4)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3 ]

            [ 0 * 0 ]
            [ 0 * 1 ]
            [ 0 * 2 ]
            [ 0 * 3 ]

            [ 0 * 0 ]
            [ 0 * 1 ]
            [ 0 * 2 ]
            [ 0 * 3 ]

            [ 0 * 3 ]
            [ 0 * 2 ]
            [ 0 * 1 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``1x5`` () =
        ! (1, 5)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3; 0 * 4 ]

            [ 0 * 0 ]
            [ 0 * 1 ]
            [ 0 * 2 ]
            [ 0 * 3 ]
            [ 0 * 4 ]

            [ 0 * 0 ]
            [ 0 * 1 ]
            [ 0 * 2 ]
            [ 0 * 3 ]
            [ 0 * 4 ]

            [ 0 * 4 ]
            [ 0 * 3 ]
            [ 0 * 2 ]
            [ 0 * 1 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``2x1`` () =
        ! (2, 1)
        |> should equal [
            [ 0 * 0 ]
            [ 1 * 0 ]

            [ 0 * 0; 1 * 0 ]

            [ 1 * 0 ]
            [ 0 * 0 ]

            [ 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``2x2`` () =
        ! (2, 2)
        |> should equal [
            [ 0 * 0; 0 * 1 ]
            [ 1 * 0; 1 * 1 ]

            [ 0 * 0; 1 * 0 ]
            [ 0 * 1; 1 * 1 ]

            [ 1 * 0 ]
            [ 0 * 0; 1 * 1 ]
            [ 0 * 1 ]

            [ 1 * 1 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``2x3`` () =
        ! (2, 3)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2 ]
            [ 1 * 0; 1 * 1; 1 * 2 ]

            [ 0 * 0; 1 * 0 ]
            [ 0 * 1; 1 * 1 ]
            [ 0 * 2; 1 * 2 ]

            [ 1 * 0 ]
            [ 0 * 0; 1 * 1 ]
            [ 0 * 1; 1 * 2 ]
            [ 0 * 2 ]

            [ 1 * 2 ]
            [ 0 * 2; 1 * 1 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``2x4`` () =
        ! (2, 4)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3 ]
            [ 1 * 0; 1 * 1; 1 * 2; 1 * 3 ]

            [ 0 * 0; 1 * 0 ]
            [ 0 * 1; 1 * 1 ]
            [ 0 * 2; 1 * 2 ]
            [ 0 * 3; 1 * 3 ]

            [ 1 * 0 ]
            [ 0 * 0; 1 * 1 ]
            [ 0 * 1; 1 * 2 ]
            [ 0 * 2; 1 * 3 ]
            [ 0 * 3 ]

            [ 1 * 3 ]
            [ 0 * 3; 1 * 2 ]
            [ 0 * 2; 1 * 1 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``2x5`` () =
        ! (2, 5)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3; 0 * 4 ]
            [ 1 * 0; 1 * 1; 1 * 2; 1 * 3; 1 * 4 ]

            [ 0 * 0; 1 * 0 ]
            [ 0 * 1; 1 * 1 ]
            [ 0 * 2; 1 * 2 ]
            [ 0 * 3; 1 * 3 ]
            [ 0 * 4; 1 * 4 ]

            [ 1 * 0 ]
            [ 0 * 0; 1 * 1 ]
            [ 0 * 1; 1 * 2 ]
            [ 0 * 2; 1 * 3 ]
            [ 0 * 3; 1 * 4 ]
            [ 0 * 4 ]

            [ 1 * 4 ]
            [ 0 * 4; 1 * 3 ]
            [ 0 * 3; 1 * 2 ]
            [ 0 * 2; 1 * 1 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``3x1`` () =
        ! (3, 1)
        |> should equal [
            [ 0 * 0 ]
            [ 1 * 0 ]
            [ 2 * 0 ]

            [ 0 * 0; 1 * 0; 2 * 0 ]

            [ 2 * 0 ]
            [ 1 * 0 ]
            [ 0 * 0 ]

            [ 2 * 0 ]
            [ 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``3x2`` () =
        ! (3, 2)
        |> should equal [
            [ 0 * 0; 0 * 1 ]
            [ 1 * 0; 1 * 1 ]
            [ 2 * 0; 2 * 1 ]

            [ 0 * 0; 1 * 0; 2 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1 ]

            [ 2 * 0 ]
            [ 1 * 0; 2 * 1 ]
            [ 0 * 0; 1 * 1 ]
            [ 0 * 1 ]

            [ 2 * 1 ]
            [ 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``3x3`` () =
        ! (3, 3)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2 ]
            [ 1 * 0; 1 * 1; 1 * 2 ]
            [ 2 * 0; 2 * 1; 2 * 2 ]

            [ 0 * 0; 1 * 0; 2 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2 ]

            [ 2 * 0 ]
            [ 1 * 0; 2 * 1 ]
            [ 0 * 0; 1 * 1; 2 * 2 ]
            [ 0 * 1; 1 * 2 ]
            [ 0 * 2 ]

            [ 2 * 2 ]
            [ 1 * 2; 2 * 1 ]
            [ 0 * 2; 1 * 1;  2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``3x4`` () =
        ! (3, 4)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3 ]
            [ 1 * 0; 1 * 1; 1 * 2; 1 * 3 ]
            [ 2 * 0; 2 * 1; 2 * 2; 2 * 3 ]

            [ 0 * 0; 1 * 0; 2 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2 ]
            [ 0 * 3; 1 * 3; 2 * 3 ]

            [ 2 * 0 ]
            [ 1 * 0; 2 * 1 ]
            [ 0 * 0; 1 * 1; 2 * 2 ]
            [ 0 * 1; 1 * 2; 2 * 3 ]
            [ 0 * 2; 1 * 3 ]
            [ 0 * 3 ]

            [ 2 * 3 ]
            [ 1 * 3; 2 * 2 ]
            [ 0 * 3; 1 * 2; 2 * 1 ]
            [ 0 * 2; 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``3x5`` () =
        ! (3, 5)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3; 0 * 4 ]
            [ 1 * 0; 1 * 1; 1 * 2; 1 * 3; 1 * 4 ]
            [ 2 * 0; 2 * 1; 2 * 2; 2 * 3; 2 * 4 ]

            [ 0 * 0; 1 * 0; 2 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2 ]
            [ 0 * 3; 1 * 3; 2 * 3 ]
            [ 0 * 4; 1 * 4; 2 * 4 ]

            [ 2 * 0 ]
            [ 1 * 0; 2 * 1 ]
            [ 0 * 0; 1 * 1; 2 * 2 ]
            [ 0 * 1; 1 * 2; 2 * 3 ]
            [ 0 * 2; 1 * 3; 2 * 4 ]
            [ 0 * 3; 1 * 4 ]
            [ 0 * 4 ]

            [ 2 * 4 ]
            [ 1 * 4; 2 * 3 ]
            [ 0 * 4; 1 * 3; 2 * 2 ]
            [ 0 * 3; 1 * 2; 2 * 1 ]
            [ 0 * 2; 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``4x1`` () =
        ! (4, 1)
        |> should equal [
            [ 0 * 0 ]
            [ 1 * 0 ]
            [ 2 * 0 ]
            [ 3 * 0 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0 ]

            [ 3 * 0 ]
            [ 2 * 0 ]
            [ 1 * 0 ]
            [ 0 * 0 ]

            [ 3 * 0 ]
            [ 2 * 0 ]
            [ 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``4x2`` () =
        ! (4, 2)
        |> should equal [
            [ 0 * 0; 0 * 1 ]
            [ 1 * 0; 1 * 1 ]
            [ 2 * 0; 2 * 1 ]
            [ 3 * 0; 3 * 1 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1; 3 * 1 ]

            [ 3 * 0 ]
            [ 2 * 0; 3 * 1 ]
            [ 1 * 0; 2 * 1 ]
            [ 0 * 0; 1 * 1 ]
            [ 0 * 1 ]

            [ 3 * 1 ]
            [ 2 * 1; 3 * 0 ]
            [ 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``4x3`` () =
        ! (4, 3)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2 ]
            [ 1 * 0; 1 * 1; 1 * 2 ]
            [ 2 * 0; 2 * 1; 2 * 2 ]
            [ 3 * 0; 3 * 1; 3 * 2 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1; 3 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2; 3 * 2 ]

            [3 * 0]
            [ 2 * 0; 3 * 1 ]
            [ 1 * 0; 2  *1; 3 * 2 ]
            [ 0 * 0; 1 * 1; 2 * 2 ]
            [ 0 * 1; 1 * 2 ]
            [ 0 * 2 ]

            [ 3 * 2 ]
            [ 2 * 2; 3 * 1 ]
            [ 1 * 2; 2 * 1; 3 * 0 ]
            [ 0 * 2; 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``4x4`` () =
        ! (4, 4)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3 ]
            [ 1 * 0; 1 * 1; 1 * 2; 1 * 3 ]
            [ 2 * 0; 2 * 1; 2 * 2; 2 * 3 ]
            [ 3 * 0; 3 * 1; 3 * 2; 3 * 3 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1; 3 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2; 3 * 2 ]
            [ 0 * 3; 1 * 3; 2 * 3; 3 * 3 ]

            [ 3 * 0 ]
            [ 2 * 0; 3 * 1 ]
            [ 1 * 0; 2 * 1; 3 * 2 ]
            [ 0 * 0; 1 * 1; 2 * 2; 3 * 3 ]
            [ 0 * 1; 1 * 2; 2 * 3 ]
            [ 0 * 2; 1 * 3 ]
            [ 0 * 3 ]

            [ 3 * 3 ]
            [ 2 * 3; 3 * 2 ]
            [ 1 * 3; 2 * 2; 3 * 1 ]
            [ 0 * 3; 1 * 2; 2 * 1; 3 * 0 ]
            [ 0 * 2; 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``4x5`` () =
        ! (4, 5)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3; 0 * 4 ]
            [ 1 * 0; 1 * 1; 1 * 2; 1 * 3; 1 * 4 ]
            [ 2 * 0; 2 * 1; 2 * 2; 2 * 3; 2 * 4 ]
            [ 3 * 0; 3 * 1; 3 * 2; 3 * 3; 3 * 4 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1; 3 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2; 3 * 2 ]
            [ 0 * 3; 1 * 3; 2 * 3; 3 * 3 ]
            [ 0 * 4; 1 * 4; 2 * 4; 3 * 4 ]

            [ 3 * 0 ]
            [ 2 * 0; 3 * 1 ]
            [ 1 * 0; 2 * 1; 3 * 2 ]
            [ 0 * 0; 1 * 1; 2 * 2; 3 * 3 ]
            [ 0 * 1; 1 * 2; 2 * 3; 3 * 4 ]
            [ 0 * 2; 1 * 3; 2 * 4 ]
            [ 0 * 3; 1 * 4 ]
            [ 0 * 4 ]

            [ 3 * 4 ]
            [ 2 * 4; 3 * 3 ]
            [ 1 * 4; 2 * 3; 3 * 2 ]
            [ 0 * 4; 1 * 3; 2 * 2; 3 * 1 ]
            [ 0 * 3; 1 * 2; 2 * 1; 3 * 0 ]
            [ 0 * 2; 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``5x1`` () =
        ! (5, 1)
        |> should equal [
            [ 0 * 0 ]
            [ 1 * 0 ]
            [ 2 * 0 ]
            [ 3 * 0 ]
            [ 4 * 0 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0; 4 * 0 ]

            [ 4 * 0 ]
            [ 3 * 0 ]
            [ 2 * 0 ]
            [ 1 * 0 ]
            [ 0 * 0 ]

            [ 4 * 0 ]
            [ 3 * 0 ]
            [ 2 * 0 ]
            [ 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``5x2`` () =
        ! (5, 2)
        |> should equal [
            [ 0 * 0; 0 * 1 ]
            [ 1 * 0; 1 * 1 ]
            [ 2 * 0; 2 * 1 ]
            [ 3 * 0; 3 * 1 ]
            [ 4 * 0; 4 * 1 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0; 4 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1; 3 * 1; 4 * 1 ]

            [ 4 * 0 ]
            [ 3 * 0; 4 * 1 ]
            [ 2 * 0; 3 * 1 ]
            [ 1 * 0; 2 * 1 ]
            [ 0 * 0; 1 * 1 ]
            [ 0 * 1 ]

            [ 4 * 1 ]
            [ 3 * 1; 4 * 0 ]
            [ 2 * 1; 3 * 0 ]
            [ 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``5x3`` () =
        ! (5, 3)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2 ]
            [ 1 * 0; 1 * 1; 1 * 2 ]
            [ 2 * 0; 2 * 1; 2 * 2 ]
            [ 3 * 0; 3 * 1; 3 * 2 ]
            [ 4 * 0; 4 * 1; 4 * 2 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0; 4 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1; 3 * 1; 4 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2; 3 * 2; 4 * 2 ]

            [ 4 * 0 ]
            [ 3 * 0; 4 * 1 ]
            [ 2 * 0; 3 * 1; 4 * 2 ]
            [ 1 * 0; 2 * 1; 3 * 2 ]
            [ 0 * 0; 1 * 1; 2 * 2 ]
            [ 0 * 1; 1 * 2 ]
            [ 0 * 2 ]

            [ 4 * 2 ]
            [ 3 * 2; 4 * 1 ]
            [ 2 * 2; 3 * 1; 4 * 0 ]
            [ 1 * 2; 2 * 1; 3 * 0 ]
            [ 0 * 2; 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``5x4`` () =
        ! (5, 4)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3 ]
            [ 1 * 0; 1 * 1; 1 * 2; 1 * 3 ]
            [ 2 * 0; 2 * 1; 2 * 2; 2 * 3 ]
            [ 3 * 0; 3 * 1; 3 * 2; 3 * 3 ]
            [ 4 * 0; 4 * 1; 4 * 2; 4 * 3 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0; 4 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1; 3 * 1; 4 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2; 3 * 2; 4 * 2 ]
            [ 0 * 3; 1 * 3; 2 * 3; 3 * 3; 4 * 3 ]

            [ 4 * 0 ]
            [ 3 * 0; 4 * 1 ]
            [ 2 * 0; 3 * 1; 4 * 2 ]
            [ 1 * 0; 2 * 1; 3 * 2; 4 * 3 ]
            [ 0 * 0; 1 * 1; 2 * 2; 3 * 3 ]
            [ 0 * 1; 1 * 2; 2 * 3 ]
            [ 0 * 2; 1 * 3 ]
            [ 0 * 3 ]

            [ 4 * 3 ]
            [ 3 * 3; 4 * 2 ]
            [ 2 * 3; 3 * 2; 4 * 1 ]
            [ 1 * 3; 2 * 2; 3 * 1; 4 * 0 ]
            [ 0 * 3; 1 * 2; 2 * 1; 3 * 0 ]
            [ 0 * 2; 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

    [<Test>]
    let ``5x5`` () =
        ! (5, 5)
        |> should equal [
            [ 0 * 0; 0 * 1; 0 * 2; 0 * 3; 0 * 4 ]
            [ 1 * 0; 1 * 1; 1 * 2; 1 * 3; 1 * 4 ]
            [ 2 * 0; 2 * 1; 2 * 2; 2 * 3; 2 * 4 ]
            [ 3 * 0; 3 * 1; 3 * 2; 3 * 3; 3 * 4 ]
            [ 4 * 0; 4 * 1; 4 * 2; 4 * 3; 4 * 4 ]

            [ 0 * 0; 1 * 0; 2 * 0; 3 * 0; 4 * 0 ]
            [ 0 * 1; 1 * 1; 2 * 1; 3 * 1; 4 * 1 ]
            [ 0 * 2; 1 * 2; 2 * 2; 3 * 2; 4 * 2 ]
            [ 0 * 3; 1 * 3; 2 * 3; 3 * 3; 4 * 3 ]
            [ 0 * 4; 1 * 4; 2 * 4; 3 * 4; 4 * 4 ]

            [ 4 * 0 ]
            [ 3 * 0; 4 * 1 ]
            [ 2 * 0; 3 * 1; 4 * 2 ]
            [ 1 * 0; 2 * 1; 3 * 2; 4 * 3]
            [ 0 * 0; 1 * 1; 2 * 2; 3 * 3; 4 * 4 ]
            [ 0 * 1; 1 * 2; 2 * 3; 3 * 4 ]
            [ 0 * 2; 1 * 3; 2 * 4 ]
            [ 0 * 3; 1 * 4 ]
            [ 0 * 4 ]

            [ 4 * 4 ]
            [ 3 * 4; 4 * 3 ]
            [ 2 * 4; 3 * 3; 4 * 2 ]
            [ 1 * 4; 2 * 3; 3 * 2; 4 * 1 ]
            [ 0 * 4; 1 * 3; 2 * 2; 3 * 1; 4 * 0 ]
            [ 0 * 3; 1 * 2; 2 * 1; 3 * 0 ]
            [ 0 * 2; 1 * 1; 2 * 0 ]
            [ 0 * 1; 1 * 0 ]
            [ 0 * 0 ]
        ]

module ``getWinLine`` =

    let X = CellType.X
    let O = CellType.O

    [<Test>]
    let ``Returns empty list for empty grid`` () =
        { Rows = 0; Cols = 0; WinLength = 0; Cells = [||] }
        |> getWinLine
        |> should be FsUnit.TopLevelOperators.Empty

    [<Test>]
    let ``Returns all win lines`` () =
        let grid = newGridFromCells 3 [
            [X; X; X]
            [X; X; X]
            [X; X; X]
        ]

        grid
        |> getWinLine
        |> should equal [
            [ { Row = 0; Col = 0; Type = X }; { Row = 0; Col = 1; Type = X }; { Row = 0; Col = 2; Type = X } ]
            [ { Row = 1; Col = 0; Type = X }; { Row = 1; Col = 1; Type = X }; { Row = 1; Col = 2; Type = X } ]
            [ { Row = 2; Col = 0; Type = X }; { Row = 2; Col = 1; Type = X }; { Row = 2; Col = 2; Type = X } ]

            [ { Row = 0; Col = 0; Type = X }; { Row = 1; Col = 0; Type = X }; { Row = 2; Col = 0; Type = X } ]
            [ { Row = 0; Col = 1; Type = X }; { Row = 1; Col = 1; Type = X }; { Row = 2; Col = 1; Type = X } ]
            [ { Row = 0; Col = 2; Type = X }; { Row = 1; Col = 2; Type = X }; { Row = 2; Col = 2; Type = X } ]

            [ { Row = 0; Col = 0; Type = X }; { Row = 1; Col = 1; Type = X }; { Row = 2; Col = 2; Type = X } ]
            [ { Row = 0; Col = 2; Type = X }; { Row = 1; Col = 1; Type = X }; { Row = 2; Col = 0; Type = X } ]
        ]

    [<Test>]
    let ``Returns all win lines for both X and O`` () =
        let grid = newGridFromCells 3 [
            [X; X; X]
            [O; O; O]
            [O; X; O]
        ]

        grid
        |> getWinLine
        |> should equal [
            [ { Row = 0; Col = 0; Type = X }; { Row = 0; Col = 1; Type = X }; { Row = 0; Col = 2; Type = X }; ]

            [ { Row = 1; Col = 0; Type = O }; { Row = 1; Col = 1; Type = O }; { Row = 1; Col = 2; Type = O }; ]
        ]

    [<Test>]
    let ``Returns all win lines of length 2 cells`` () =
        let grid = newGridFromCells 2 [
            [X; X; X]
            [O; O; O]
            [O; X; O]
        ]

        grid
        |> getWinLine
        |> should equal [
            [ { Row = 0; Col = 0; Type = X }; { Row = 0; Col = 1; Type = X } ]
            [ { Row = 0; Col = 1; Type = X }; { Row = 0; Col = 2; Type = X } ]

            [ { Row = 1; Col = 0; Type = O }; { Row = 1; Col = 1; Type = O } ]
            [ { Row = 1; Col = 1; Type = O }; { Row = 1; Col = 2; Type = O } ]

            [ { Row = 1; Col = 0; Type = O }; { Row = 2; Col = 0; Type = O } ]
            [ { Row = 1; Col = 2; Type = O }; { Row = 2; Col = 2; Type = O } ]

            [ { Row = 1; Col = 1; Type = O }; { Row = 2; Col = 2; Type = O } ]

            [ { Row = 1; Col = 1; Type = O }; { Row = 2; Col = 0; Type = O } ]
        ]

    [<Test>]
    let ``Returns nothing for all Empty cells`` () =
        newGrid (3, 3) 3
        |> getWinLine
        |> should be FsUnit.TopLevelOperators.Empty

    [<Test>]
    let ``Returns empty list when no win lines`` () =
        let grid = newGridFromCells 3 [
            [ X; O; X ]
            [ O; X; O ]
            [ O; X; O ]
        ]

        grid
        |> getWinLine
        |> should be FsUnit.TopLevelOperators.Empty

module ``getGridState`` =

    let X = CellType.X
    let O = CellType.O

    [<Test>]
    let ``Returns Draw for empty grid`` () =
        { Rows = 0; Cols = 0; WinLength = 0; Cells = [||] }
        |> getGridState
        |> should equal Draw

    [<Test>]
    let ``Returns Begining for grid of Empty cells`` () =
        newGrid (3, 3) 3
        |> getGridState
        |> should equal Begining

    [<Test>]
    let ``Returns playable for grid with some Empty cells`` () =
        newGridFromCells 3 [
            [ X; O; X ]
            [ O; X; O ]
            [ CellType.Empty; X; O ]
        ]
        |> getGridState
        |> should equal Playable

    [<Test>]
    let ``Returns draw for grid without Empty cells and without win lines`` () =
        newGridFromCells 3 [
            [ X; O; X ]
            [ O; X; O ]
            [ O; X; O ]
        ]
        |> getGridState
        |> should equal Draw

    [<Test>]
    let ``Returns XWon for x won lines`` () =
        newGridFromCells 3 [
            [ X; O; X ]
            [ O; X; O ]
            [ O; X; X ]
        ]
        |> getGridState
        |> should equal (XWon [ { Row = 0; Col = 0; Type = X }; { Row = 1; Col = 1; Type = X }; { Row = 2; Col = 2; Type = X } ])

    [<Test>]
    let ``Returns OWon for o won lines`` () =
        newGridFromCells 3 [
            [ X; O; X ]
            [ X; O; O ]
            [ O; O; X ]
        ]
        |> getGridState
        |> should equal (OWon [ { Row = 0; Col = 1; Type = O }; { Row = 1; Col = 1; Type = O }; { Row = 2; Col = 1; Type = O } ])

    [<Test>]
    let ``Returns draw when grid contains X and O win lines`` () =
        newGridFromCells 3 [
            [ X; X; X ]
            [ O; X; O ]
            [ O; O; O ]
        ]
        |> getGridState
        |> should equal Draw

module ``withCellAt`` =

    [<Test>]
    let ``Successfully replaces cell`` () =
        let grid = newGrid (3, 3) 3
        for row in 0..2 do
            for col in 0..2 do
                let cell =
                    withCellAt row col CellType.X grid
                    |> cellAt row col

                cell.Row |> should equal row
                cell.Col |> should equal col
                cell.Type |> should equal CellType.X

module ``withMoveAt`` =

    [<Test>]
    let ``Returns new grid with updated cell`` () =
        let grid = newGrid (3, 3) 3

        grid.Cells
        |> Seq.ofArray
        |> Seq.map (fun cell -> (cell, withMoveAt cell MoveAs.X grid, withMoveAt cell MoveAs.O grid))
        |> Seq.iter (fun (cell, xMove, oMove) ->
            xMove
            |> cellAt cell.Row cell.Col
            |> should equal { cell with Type = CellType.X }

            oMove
            |> cellAt cell.Row cell.Col
            |> should equal { cell with Type = CellType.O })

        grid
        |> should equal (newGrid (3, 3) 3)

module ``enumerateEmptyCells`` =

    [<Test>]
    let ``Returns empty cells only`` () =
        [
            [ CellType.X; CellType.O; CellType.Empty ]
            [ CellType.O; CellType.Empty; CellType.Empty ]
            [ CellType.Empty; CellType.Empty; CellType.Empty ]
        ]
        |> newGridFromCells 3
        |> enumerateEmptyCells
        |> should equal
        <| Seq.ofList [
            { Row = 0; Col = 2; Type = CellType.Empty }
            { Row = 1; Col = 1; Type = CellType.Empty }
            { Row = 1; Col = 2; Type = CellType.Empty }
            { Row = 2; Col = 0; Type = CellType.Empty }
            { Row = 2; Col = 1; Type = CellType.Empty }
            { Row = 2; Col = 2; Type = CellType.Empty }
        ]

module ``oppositeMove`` =

    [<Test>]
    let ``X -> O`` () =
        MoveAs.X
        |> oppositeMove
        |> should equal MoveAs.O

    [<Test>]
    let ``O -> X`` () =
        MoveAs.O
        |> oppositeMove
        |> should equal MoveAs.X


module ``isLimitReached`` =

    [<Test>]
    let ``Unlimited returns false`` () =
        limitUnlimited
        |> isLimitReached
        |> should equal false

    [<Test>]
    let ``Depth returns false when max depth is reached`` () =
        [ 1 .. 1001 ]
        |> Seq.fold (fun limit _ -> nextLimit limit) (limitByDepth 1001)
        |> isLimitReached
        |> should equal true

    [<Test>]
    let ``This test should end in 2 seconds`` () =
        let mutable limit = limitByTime <| System.TimeSpan.FromSeconds 2

        while not <| isLimitReached limit do
            limit <- nextLimit limit

module ``limitByTime`` =

    [<Test>]
    let ``Should throw en error if time is negative `` () =
        (fun () -> limitByTime <| System.TimeSpan.FromSeconds -1 |> ignore)
        |> should throw typeof<System.Exception>

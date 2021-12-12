module TestLibTicTacToe.Tests.TestHelpers

open LibTicTacToe.Helpers
open NUnit.Framework
open FsUnit

module ``isAllEqualBy`` =

    [<Test>]
    let ``For empty list returns true`` () =
        isAllEqualBy id ([] : int list)
        |> should be True

    [<Test>]
    let ``For single element list returns true`` ([<Random(10)>]a: int) =
        isAllEqualBy id [a]
        |> should be True

    [<Test>]
    let ``For list of equal elements returns true`` ([<Range(2, 10)>]size: int) ([<Random(1)>] value: int) =
        List.replicate size value
        |> isAllEqualBy id
        |> should be True

    [<Test>]
    let ``For list of different elements returns false`` ([<Range(2, 10)>] size: int) =
        isAllEqualBy id [ 0 .. size ]
        |> should be False

    [<Test>]
    let ``Selector function is called`` () =
        (fun () ->
            isAllEqualBy (fun _ -> failwith "Ooops") [1; 2; 3] |> ignore)
        |> should throw typeof<System.Exception>

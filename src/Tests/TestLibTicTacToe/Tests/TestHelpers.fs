module TestLibTicTacToe.Tests.TestHelpers

open NUnit.Framework
open FsUnit

open LibTicTacToe.Helpers


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

module ``tryMaxBy`` =

    [<Test>]
    let ``For empty seq returns None`` () =
        (Seq.empty: int seq)
        |> tryMaxBy id
        |> should equal None

    [<Test>]
    let ``For random seq returns Some max element`` ([<Range(1, 10)>] size: int) =
        let numbers =
            [ 0 .. size ]
            |> Seq.ofList
            |> Seq.map (fun _ -> NUnit.Framework.TestContext.CurrentContext.Random.Next ())
            |> Seq.cache

        numbers
        |> tryMaxBy id
        |> should equal (Some (Seq.max numbers))


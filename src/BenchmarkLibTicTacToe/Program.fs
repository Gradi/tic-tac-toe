module BenchmarkLibTicTacToe.Program

open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs


[<EntryPoint>]
let main argv =

    let config = ManualConfig.Create(DefaultConfig.Instance)
                     .AddDiagnoser (MemoryDiagnoser.Default)

    BenchmarkRunner.Run<BenchmarkBotLogic> (config) |> ignore
    0

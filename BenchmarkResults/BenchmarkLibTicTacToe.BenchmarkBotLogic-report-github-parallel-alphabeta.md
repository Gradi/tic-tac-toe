``` ini

BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19043.1320 (21H1/May2021Update)
Intel Core i7-4771 CPU 3.50GHz (Haswell), 1 CPU, 8 logical and 4 physical cores
.NET SDK=6.0.100
  [Host]     : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT DEBUG
  DefaultJob : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT


```
|          Method |     Mean |   Error |  StdDev |       Gen 0 |    Gen 1 | Allocated |
|---------------- |---------:|--------:|--------:|------------:|---------:|----------:|
|     GetBestMove | 133.0 ms | 1.63 ms | 1.52 ms |  89250.0000 | 250.0000 |    356 MB |
| RunGridTillDraw | 160.2 ms | 2.77 ms | 2.59 ms | 106750.0000 | 250.0000 |    425 MB |

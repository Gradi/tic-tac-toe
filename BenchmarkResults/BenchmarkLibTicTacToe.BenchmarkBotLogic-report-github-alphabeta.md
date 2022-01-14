``` ini

BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19043.1320 (21H1/May2021Update)
Intel Core i7-4771 CPU 3.50GHz (Haswell), 1 CPU, 8 logical and 4 physical cores
.NET SDK=6.0.100
  [Host]     : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT DEBUG
  DefaultJob : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT


```
|          Method |     Mean |   Error |  StdDev |       Gen 0 | Allocated |
|---------------- |---------:|--------:|--------:|------------:|----------:|
|     GetBestMove | 213.3 ms | 1.01 ms | 2.09 ms |  89000.0000 |    356 MB |
| RunGridTillDraw | 249.9 ms | 0.92 ms | 0.82 ms | 106000.0000 |    425 MB |

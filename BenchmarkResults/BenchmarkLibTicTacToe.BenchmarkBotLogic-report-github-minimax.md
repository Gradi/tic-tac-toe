``` ini

BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19043.1320 (21H1/May2021Update)
Intel Core i7-4771 CPU 3.50GHz (Haswell), 1 CPU, 8 logical and 4 physical cores
.NET SDK=6.0.100
  [Host]     : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT DEBUG
  DefaultJob : .NET 6.0.0 (6.0.21.52210), X64 RyuJIT


```
|          Method |    Mean |    Error |   StdDev |        Gen 0 | Allocated |
|---------------- |--------:|---------:|---------:|-------------:|----------:|
|     GetBestMove | 3.715 s | 0.0201 s | 0.0157 s | 1568000.0000 |      6 GB |
| RunGridTillDraw | 4.207 s | 0.0228 s | 0.0214 s | 1763000.0000 |      7 GB |

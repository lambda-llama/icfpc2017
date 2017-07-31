# icfpc2017
λ-llama code for ICFP contest 2017 

## How to work with the code

All commands assume cwd in `src` at the start.

Restore packages each time dependencies inside `.fsproj` change.
```
dotnet restore
```

You can build from `src` or any of the project folders, dependencies are resolved properly in both cases.
```
dotnet build
```
You can also restore project folders, too.

To run the app use `dotnet`, not `dotnet run`, which is _slow_:
```
# with a helper script
Icfpc2017.App/run
Icfpc2017.App/run release
# or without
dotnet Icfpc2017.App/bin/Debug/netcoreapp1.1/Icfpc2017.App.dll
dotnet Icfpc2017.App/bin/Release/netcoreapp1.1/Icfpc2017.App.dll
```

To run the tests:
```
dotnet test Icfpc2017.Tests
# or
cd Icfpc2017.Tests
dotnet test
```

`dotnet` commands are pretty discoverable, you can add `--help` at any level:
```
dotnet --help
dotnet new --help
dotnet new console --help
```
Sometimes, however, commands complain at `--help` and print help when not supplied any arguments, so in case `--help` fails, try without it. E.g.
```
dotnet exec
```

Things to remember/common gotchas:
1. Files must be manually added to `.fsproj` fiels and ordered by hand, dependencies first.
2. p.1 and language design in other places make type dependency cycles impossible to compile, so plan/design accordingly.
3. Default F# `Array` and `List` are not the same as in C#, you need to fully qualify the latter.

#!/bin/sh
if [[ $# -eq 0 ]]; then
    dotnet Icfpc2017.App/bin/Release/netcoreapp2.0/Icfpc2017.App.dll
else
    dotnet Icfpc2017.App/bin/$1/netcoreapp2.0/Icfpc2017.App.dll ${@:2}
fi

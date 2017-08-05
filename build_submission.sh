#! /bin/bash

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
SUBMISSION_ID=$1

# output
OUTPUT_DIR=$SCRIPT_DIR/submission
OUTPUT_BIN_DIR=$OUTPUT_DIR/bin
OUTPUT_SRC_DIR=$OUTPUT_DIR/src

# input
STATIC_DIR=$SCRIPT_DIR/static
APP_SRC_DIR=$SCRIPT_DIR/src/Icfpc2017.App
APP_BIN_DIR=$APP_SRC_DIR/bin/Release/netcoreapp2.0

rm -rf $OUTPUT_DIR
mkdir -p $OUTPUT_BIN_DIR
mkdir -p $OUTPUT_SRC_DIR
cp $STATIC_DIR/* $OUTPUT_DIR/

pushd $APP_SRC_DIR > /dev/null
dotnet restore
dotnet publish -c Release -o $OUTPUT_BIN_DIR
popd > /dev/null

cp -r $APP_SRC_DIR/*.fs* $OUTPUT_SRC_DIR/

pushd $OUTPUT_DIR > /dev/null
tar -czf "icfp-$SUBMISSION_ID.tar.gz" *
popd > /dev/null

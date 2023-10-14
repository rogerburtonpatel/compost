#!/bin/bash

for inFile in $(find tests -type f -iname "*.in") ; do
    inBase=$(basename $inFile)
    outFile="tests/${inBase%.*}.out"
    [ ! -f $outFile ] && outFile=$inFile # if in = out, don't need out
    echo "Running test: "$inFile $outFile
    diff <(cat $inFile) $outFile
    if [ "$?" -ne "0" ];  then
        >&2 echo "Test $inFile $outFile failed above"
    fi
done
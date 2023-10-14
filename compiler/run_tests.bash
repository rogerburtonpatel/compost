#!/bin/bash

SCRIPT=$(realpath "$0")
BASEDIR=$(dirname "$SCRIPT")

TESTDIR=$BASEDIR/tests/

shopt -s nullglob


build ()
{
    cd "$BASEDIR"
    dune build 
}

numtests=0
numfail=0
# for people wanting to make changes: $1, $2 are arguments to a function
update_failure () { 
    failure=$1
    testname=$2
    out=$3
    
    if [[ "$failure" -ne "0" ]]; then 
        >&2 echo "Failed test $testname"
        >&2 echo "Output: "
        >&2 echo "$out"
        numfail=$((numfail + 1))
    fi
    
}


run_tests () {

    for inFile in $(find $BASEDIR/tests -type f -iname "*.in") ; do
        
        numtests=$((numtests + 1))
        inBase=$(basename $inFile)
        outFile="tests/${inBase%.*}.out"
        inFile=$(basename "$(dirname "$inFile")")/$(basename "$inFile")
        [ ! -f $outFile ] && outFile=$inFile # if in = out, don't need out
        echo "Running test: "diff $inFile $outFile
        toplevelOut=$(dune exec toplevel $inFile 2>&1)
        out=$(diff -wy <(echo "$toplevelOut") <(cat "$outFile") 2>&1)
        update_failure "$?" $inFile "${out%x}"

    done

    if [[ "$numtests" -eq "0" ]]; then
        >&2 echo "No tests run. Check why that might be."
        exit 1
    fi

    if [[ "$numfail" -eq "0" ]]; then 
        echo "All tests passed."
    else 
        numpassed=$((numtests - $numfail))
        echo "$numpassed / $numtests tests passed. \
See above output for details."
    exit 1
    fi
}

main () {
    build
    run_tests
}

main

shopt -u nullglob


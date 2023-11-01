#!/bin/bash

SCRIPT=$(realpath "$0")
BASEDIR=$(dirname "$SCRIPT")

TESTDIR=$BASEDIR/tests/

shopt -s nullglob

# change if your sed is different! on halligan this will be /usr/bin/sed. 
if [ `uname` = "Darwin" ]; then
    SED=/opt/homebrew/bin/gsed
else 
    SED=sed
fi

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
    pout=$3
    eout=$4
    out=$5
    
    if [[ "$failure" -ne "0" ]]; then 
        >&2 echo "Failed test $testname"
        >&2 echo "Output: "
        >&2 echo "$pout"
        >&2 echo "Expected: "
        >&2 echo "$eout"
        >&2 echo "Difference: "
        >&2 echo "$out"
        numfail=$((numfail + 1))
    fi
    
}


run_tests () {

    for inFile in $(find $BASEDIR/tests -type f -iname '*.in') ; do
        
        numtests=$((numtests + 1))
        inBase=$(basename $inFile)
        outFile="tests/${inBase%.*}.out"
        inFile=$(basename "$(dirname "$inFile")")/$(basename "$inFile")
        [ ! -f $outFile ] && outFile=$inFile # if in = out, don't need out
        echo "Running test: "diff $inFile $outFile
        toplevelOut=$(dune exec -- compost -a $inFile 2>&1)
        toplevelExpected=$(cat "$outFile")
        out=$(diff -wy <(echo "$toplevelOut") <(echo "$toplevelExpected") 2>&1)
        update_failure "$?" $inFile "${toplevelOut%x}" "${toplevelExpected%x}" "${out%x}"

    done

    if [[ "$numtests" -eq "0" ]]; then
        >&2 echo "No tests run. Check why that might be."
        exit 1
    fi

    if [[ "$numfail" -eq "0" ]]; then 
        echo "All $numtests tests passed."
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


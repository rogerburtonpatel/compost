#!/usr/bin/env bash

SCRIPT=$(realpath "$0")
BASEDIR=$(dirname "$SCRIPT")

TESTDIR=$BASEDIR/tests

shopt -s nullglob

# System dependent binaries
if [ `uname` = "Darwin" ]; then
    SED=/opt/homebrew/bin/gsed
else 
    SED=sed
fi

build ()
{
    pushd $BASEDIR > /dev/null
    dune build 
    popd > /dev/null
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

compost () {
    comFile=$1
    inFile=$2
    shift
    shift
    pushd $BASEDIR > /dev/null
    dune exec compost -- "$@" "$comFile"
    popd > /dev/null
}

compileAndRun () {
    comFile=$1
    inFile=$2
    shift
    shift
    pushd $BASEDIR > /dev/null
    outBin=$(mktemp)
    ./compile-compost "$comFile" "$outBin"
    $outBin < "$inFile"
    rm "$outBin"
    popd > /dev/null
}

run_tests () {
    suite="$1"
    comFiles="$2"
    testFn="$3"
    shift
    shift
    shift
    testFnArgs=("$@")

    pushd "${TESTDIR}/${suite}" > /dev/null

    for comFile in $(find . -type f -iname "$comFiles"); do
        numtests=$((numtests + 1))
        comFile=$(basename $comFile)
        outFile="${comFile%.*}.out"
        inFile="${comFile%.*}.in"
        [ ! -f $outFile ] && outFile=$comFile
        [ ! -f $inFile ] && inFile="/dev/null"
        echo "Running test ${numtests}: ${suite} (${comFile}, ${outFile}, ${inFile})"
        [ "$inFile" != "/dev/null" ] && inFile="${TESTDIR}/${suite}/${inFile}"
        runEval=$("${testFn}" "${TESTDIR}/${suite}/${comFile}" "${inFile}" "${testFnArgs[@]}" 2>&1)
        # THIS WILL WRITE THE OUTPUT FILE
        # echo "${runEval}" > "${outFile}"
        outExp=$(cat "${outFile}")
        out=$(diff -wy <(echo "$runEval") <(echo "$outExp") 2>&1)
        update_failure "$?" $comFile "${runEval%x}" "${outExp%x}" "${out%x}"
    done

    popd > /dev/null
}

main () {
    build

    comFiles="*.com"
    if [ -n "$2" ] ; then
        comFiles="$2"
    fi

    case $1 in
      -a) run_tests ast "$comFiles" compost -a ;;
      -p) run_tests past "$comFiles" compost -p ;;
      -u) run_tests uast "$comFiles" compost -u ;;
      -t) run_tests tast "$comFiles" compost -t ;;
      -m) run_tests mast "$comFiles" compost -m ;;
      -c) run_tests llvm "$comFiles" compost -c ;;
      -r) run_tests run "$comFiles" compileAndRun ;;
      --all | *) run_tests ast "$comFiles" compost -a ;
                 run_tests past "$comFiles" compost -p ;
                 run_tests uast "$comFiles" compost -u ;
                 run_tests tast "$comFiles" compost -t ;
                 run_tests mast "$comFiles" compost -m ;
                 run_tests llvm "$comFiles" compost -c ;
                 run_tests run "$comFiles" compileAndRun ;;
    esac

    if [[ "$numtests" -eq "0" ]]; then
        >&2 echo "No tests run. Check why that might be."
        exit 1
    fi

    if [[ "$numfail" -eq "0" ]]; then 
        echo "All $numtests tests passed."
    else 
        numpassed=$((numtests - $numfail))
        echo "$numpassed / $numtests tests passed."
        echo "See above output for details."
        exit 1
    fi
}

main "${@}"

shopt -u nullglob


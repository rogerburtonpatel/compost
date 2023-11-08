#!/bin/bash

SCRIPT=$(realpath "$0")
BASEDIR=$(dirname "$SCRIPT")

TESTDIR=$BASEDIR/tests

shopt -s nullglob

# System dependent binaries
if [ `uname` = "Darwin" ]; then
    SED=/opt/homebrew/bin/gsed
    LLI=lli-14
else 
    SED=sed
    LLI=lli-14
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
    pushd $BASEDIR > /dev/null
    dune exec compost -- "$@"
    popd > /dev/null
}

compostrun () {
    compost "$@" | $LLI
}

run_tests () {
    suite="$1"
    inFiles="$2"
    shift
    shift
    testFn=("$@")

    pushd "${TESTDIR}/${suite}" > /dev/null

    for inFile in $(find . -type f -iname "$inFiles"); do
        numtests=$((numtests + 1))
        inFile=$(basename $inFile)
        outFile="${inFile%.*}.out"
        [ ! -f $outFile ] && outFile=$inFile
        echo "Running test ${numtests}: ${suite} (${inFile}, ${outFile})"
        inEval=$("${testFn[@]}" "${TESTDIR}/${suite}/${inFile}" 2>&1)
        outExp=$(cat "${outFile}")
        out=$(diff -wy <(echo "$inEval") <(echo "$outExp") 2>&1)
        update_failure "$?" $inFile "${inEval%x}" "${outExp%x}" "${out%x}"
    done

    popd > /dev/null

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

main () {
    build

    inFiles="*.com"
    if [ -n "$2" ] ; then
        inFiles="$2"
    fi

    case $1 in
      -a) run_tests ast "$inFiles" compost -a ;;
      -d) run_tests uast "$inFiles" compost -d ;;
      -l) run_tests llvmir "$inFiles" compost -l ;;
      -c) ;&
       *) run_tests compile "$inFiles" compostrun -c ;;
    esac
}

main "${@}"

shopt -u nullglob


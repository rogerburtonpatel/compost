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
    compost "$@" | lli-14
}

run_tests () {
    TESTDIR=$1
    shift
    TESTFN=("$@")

    pushd "${BASEDIR}/tests/${TESTDIR}" > /dev/null

    # Grab a specific file if specified on command line, or else 
    # all appropriate files 
    inFiles=$(find . -type f -iname '*.com') 
    if [ -n "$3" ] ; then 
        inFiles=$(find . -type f -iname "$3")
    fi

    for inFile in $inFiles; do 
        numtests=$((numtests + 1))
        inFile=$(basename $inFile)
        outFile="${inFile%.*}.out"
        [ ! -f $outFile ] && outFile=$inFile
        echo "Running test: ${TESTDIR} ${numtests} (${inFile}, ${outFile})"
        inEval=$("${TESTFN[@]: 0:2}" "${BASEDIR}/tests/${TESTDIR}/${inFile}" 2>&1)
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

    case $1 in
      -a) run_tests ast compost -a $2 ;;
      -d) run_tests uast compost -d $2 ;;
      -l) run_tests llvmir compost -l $2 ;;
      -c) ;&
       *) run_tests compile compostrun -c $2 ;;
    esac
}

main "${@}"

shopt -u nullglob


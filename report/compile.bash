#! /bin/bash

pdflatex --shell-escape Compost.tex 
pdflatex --shell-escape Compost.tex
pdflatex --shell-escape Compost.tex

rm *.out
rm *.aux
rm *.out
rm *.toc
rm *.log
rm -r _minted-Compost
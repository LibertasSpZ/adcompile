#!/usr/bin/env bash

ROOT=$pwd

mkdir QAOAoutputs

mkdir QNNoutputs

mkdir VQEoutputs

mkdir VQCIfAdvantageoutputs

for x in ./testExamples/QAOA/*.prog; do ./adcompile $x t1 > QAOAoutputs/`basename $x .prog`.out ; done

    ## one may feel free to change t1 to any parameter of the form "tn" where n is an integer.
    ## one may also make their own new test cases following the input program format, and modify the script
    ## locally accordingly.

for x in ./testExamples/QNN/*.prog; do ./adcompile $x t1 > QNNoutputs/`basename $x .prog`.out ; done


for x in ./testExamples/VQE/*.prog; do ./adcompile $x t1 > VQEoutputs/`basename $x .prog`.out ; done


for x in ./testExamples/VQCIfAdvantage/*.prog; do ./adcompile $x t1 > VQCIfAdvantageoutputs/`basename $x .prog`.out ; done
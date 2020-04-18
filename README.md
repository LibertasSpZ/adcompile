## Quantum Autodifferentiation Parser and Compiler

This repository contains the instructions for running and the source code organization for a quantum autodifferentiation parser and compiler, which is the primary software artifact for the paper
  
*Shaopeng Zhu, Shih-Han Hung, Shouvanik Chakrabarti, and Xiaodi Wu.  On the Principles of Quantum Differential Programming Languages.* [PLDI 2020](https://pldi20.sigplan.org/details/pldi-2020-papers/51/On-the-Principles-of-Differential-Quantum-Programming-Languages), [arXiv: 2004.01122](https://arxiv.org/abs/2004.01122).


  
In particular, we implemented the code-transformation rules, and the compilation of additive quantum programs in Section 6 and 4 resepectively of the paper. We evaluate the artifact in Section 8.2 of the paper on a selection of representative quantum programs for quantun machine learning. 

### Source-File Structure

`adlexer.mll` and `adparser.mly` parses strings in an input file into tokens; `ast.ml` contains the typing information of the tokens. `compiler.ml` implements the code transformation and code compilation rules of the paper, with `codeTransformation` and `codeCompilation` the respective main functions. Lastly, `main.ml` provides an interface connecting the input, lexer, parser, compiler and output.

### Getting Started: Requirements

* [Homebrew](https://brew.sh/) (for macOS)
* [opam](https://opam.ocaml.org/)
* [OCaml,ocamlfind](https://ocaml.org/)

In particular on macOS, for example, first issue the command

```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

to install Homebrew; then issue 

```
brew install ocaml
brew install opam
brew install ocaml-findlib
opam init
```

to install opam and OCaml. For installation instructions of opam and OCaml on other operating systems please check the websites listed at the beginning. 


### Getting Started: Setup

It is a few lines of common commands to install our quantum autodifferentiation parser and compiler. After cloning the repository, simply issue the commands 

```
cd QAutoDiff/adcompile
make exe
```

This will generate an executible named `adcompile` in the directory `~/QAutoDiff/adcompile`. 

### Differentiating quantum programs using `adcompile`

Given an original parameterized quantum program (as defined in Section 3), one may now compute and print the additive differential program (after applying code-transformaton rules in Setion 6, before compilation) as well as the compiled multiset (after compilation in Section 4) of differential programs, using the executible `adcompile`. One may also read out the size of the multiset from the output file. We have provided some sample programs in `~/QAutoDiff/adcompile/testExamples`. 

Assuming you are in the directory `~/QAutoDiff/adcompile/`, the command for manual differentiation of a parameterized program with respect to a parameter is of the form

```
./adcompile arg1 arg2
```

where arg1 is the test program file name, and arg2 is the parameter with respect to which you would like to differentiate your test program. For example, again assuming you have `cd`-ed to the directory `~/QAutoDiff/adcompile/`, then 

```
./adcompile testExamples/QNN/QNN_medium_if.prog t1 > YourDesiredOutputName.out
```

differentiates the test program "`QNN_medium_if.prog`" (a medium sized quantum neural net program, with case control) with respect to the parameter `t1`.

### Using the testrunner.sh script to demonstrate output

We made a driver script to run the compiler on the test programs we wrote. To run this driver, please issue

```
./testrunner.sh
```

in the working directory (namely `~/QAutoDiff/adcompile/`).


### Results and remarks

We evaluate our artifact on a selection of representative test programs (in `~/testExamples`) as described in Section 8.2 of the paper.  Please refer to Table 2 from the evaluation part (Section 8.2) of our paper for details. Some explanations include: 

* when counting the number of lines in the input programs, we did not count the empty lines.
* One should note that all columns in Table 2 except $|\#\frac{\partial}{\partial\theta}(\bullet)|$ are describing the input files; $|\#\frac{\partial}{\partial\theta}(\bullet)|$, counting the number of parameterized programs in the compiled multiset, is recorded at the end of each output file.

  For example, in the input file `QNN_medium_if.prog`, one may observe there are 6 * 3 + 5 + 4 + 3 + 2 + 1 = 33 lines of code in each basic block, so total number of lines of code is 33 * 5 (blocks) + 18 (initialization) + 6 (case control sentences) = 189 lines of code, as Table 2 recorded. Among these lines of code, only the "block" code has one gate per line, so 33 * 5 = 165 gates. (Note that by "gate" we mean unitary gates, so the initialization does not count as gates). We have three layers network using 2 case control, and 18 qubits in the system (as witnessed by the first 18 lines of initialization code). Number of occurences of t1 in each block is 8, so the total number of occurances of t1 is 3 * 8 = 24 (intuitively after passing each case-control only 1 branch is executed, so the occurence of t1 only grows by 8 instead of by 2 * 8 = 16. See also Definition 7.1 in the paper). The "lines, gates, layers, qb, OC" information for all other input programs were similarly computed by hand.
  
  The $|\#\frac{\partial}{\partial\theta}(\bullet)|$ of the differential program of `QNN_medium_if.prog` is recorded in Line 17752 of `res_QNN_medium_if.txt` from the `~/QAutoDiff/adcompile/testExamples/result/QNNRes` directory of this git repository (alternatively, one may look at the `YourDesiredOutputName.out` file you obtained by running the `./adcompile testExamples/QNN/QNN_medium_if.prog t1 > YourDesiredOutputName.out` command; yet another option: check the `~/QAutoDiff/adcompile/QNNoutputs/QNN_medium_if.out` file generated by running the `./testrunner.sh` script in the `~/QAutoDiff/adcompile` directory, which produces 4 output directories holding the output files). The number of programs in the compiled multiset of the differential program of `QNN_medium_if.prog` is 24, agreeing with Table 2. One may check that $|\#\frac{\partial}{\partial\theta}(\bullet)|$ from all the other rows also agree with Table 2 by eyeballing the last few lines of each output program.
  
  If one makes their own test program, run `./adcompile yourOwnTest.prog tk > yourOwnOutput.out` (where you should replace `tk` with the desired parameter legally coded, such as `t3`). One may then count the occurence of `tk` in `youOwnTest.prog`, and observe that the number of occurences of `tk` is always greater than or equal to $|\#\frac{\partial}{\partial\theta}(\bullet)|$ recorded in `yourOwnOutput.out`, which agrees with the theoretical bound in the paper (Proposition 7.2).

* some of the large scale programs may take up to around 90 seconds on a 1.3 GHz Intel Core i5 processor with 8 GB 1867 MHz LPDDR3 memory to differentiate. The script `testrunner.sh` takes less than 3 min on a 3 GHz Intel Core i7 processor with 8 GB 1600 MHz DDR3 memory (real time real	2m43.778s on a trial run).
* We identify `Skip[q1, q2]` with `Skip[q2,q1]`, `Abort[q1,q2]` with `Abort[q2,q1]`, due to unanimous behavior across all registers.
* One is welcomed to make their own test programs following the input program format exemplified by the existing ones.


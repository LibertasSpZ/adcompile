# adcompiler

 This is an autodifferentiation parser and compiler as implementation of the PLDI submission ``On the Principles of Quantum Differential Programming Languages". It computes and prints the multiset of differential programs given an original parameterized quantum program, then prints the size of the multiset.


To run the compiler, issue the command 

```
make exe
```

in your terminal with working directory adcompile. Then type 

```
adcompiler arg1 arg2
```

where arg1 is the test program file name, arg2 is the parameter with respect to which you would like to differentiate your test program.

E.g.:

```
adcompiler testExamples/QNN/QNN_medium_if.txt t1
```

Have fun differentiating!:)

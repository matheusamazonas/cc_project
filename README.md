# Compiler Construction project

This is our project for the 2017 Compiler Construction course.
We developed a compiler for the Simple Programming Language (SPL) given in the course, and extended it with higher-order functions, higher-rank type inference/checking, and nested functions with variable capture, as well as more minor changes.

Our code can be run either from the command line or from within WinGHCi using ":set args". Through either mechanism, main.lhs's main method is invoked with one of the execution modes (**c**ompile to standard output, **m**ake file, e**x**ecute after make, or **d**ebug by showing both code and execution) as first argument, and a file name as second argument. For example, `./main x ../examples/test.spl` in a terminal or equivalently `:set args "x" "..\\examples\\test.spl"` in WinGHCi executes a file `test.spl` in the specified directory path. Note: Showing the entire code is no longer recommended due to the size of the standard library, so execution modes `c` and `d` are not recommended.

**Important:** because of the size of the standard library (owing mainly to runtime exception messages), the original ssm.jar file no longer offers enough space for the code and stack segment. As a result, it quickly overflows into the heap. Thus, we use a different version of ssm.jar included in the repository, which has (much) more stack and heap space, and which lacks bugs in integer printing code and GUI updates present in the original file. This version has been provided to us by Thomas Churchman (also in the course).

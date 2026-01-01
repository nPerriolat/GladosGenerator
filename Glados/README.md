# GLADOS Project

The GLADOS project is an exercise in designing a fully-fledged programming language, along with its own tools to make it really usable.  
The project will be entirely coded in Haskell, a functional programming language.  
Here's the [subject](subjects/B-FUN-500_GLaDOS.pdf).  

The 2 months of work were split in two parts :  
1) An interpreter for a subset of the Scheme programming language
2) Designing our own programming language and bytecode to make a compiler and a VM

## 1- Interpreter

The interpreter had to be dropped for the second part of the project, but it is still visible and usable in the `evaluate` branch of this repository.  
Feel free to read the subject to see what features were exactly required.  

## 2- Compiler & VM

### 2.1. PDP language

In accordance to the subject, we designed a brand new language called PDP.  
PDP is a functional language, broadly inspired by Scheme, with many features and a decent standard library (mathematically oriented).  
Click [here](Langage.md) see the entire language documentation.  

### 2.2. Pseudo-Assembly / Bytecode

Our PDP language needs to be compiled into bytecode, thus we had to design a pseudo-assembly capable of representing every PDP instruction.  
It's a very small of barely a ten of instructions, pretty similar to real-world x86 assembly, with some type indications.  
The pseudo-assembly syntax is described [here](ASM-Doc.md).  
There is also a rough example of what a factorial function would look like in this pseudo-assembly [here](asm-example.md).  

### 2.3. VM

We also provide a VM which will execute a compiled PDP bytecode file.

## Repository tree
- app : main Haskell file
- examples / examples-expected : interpreter functional tests, respectively Scheme inputs and their expected output
- src : all source code expect main
- subjects : project subject and its parsing bootstrap
- test : unit tests

## Building commands
- make : builds the compiler and the VM, without unit tests
- make clean : removes Haskell object files, and keeps the executable
- make fclean : make clean + removes the executable
- make re : make fclean + make
- make unit_test : compiles and runs executable tests and coverage
- make func_test : starts interpreter functional tests
- make tests : make unit_tests + make func_test

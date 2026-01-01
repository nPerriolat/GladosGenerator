# GladosGenerator

## A. Generator testing

To build the project use the Makefile located at the root. Use `make` or `make re`.

Before running the project, please verify you have the needed rules files.

You can either run the program by specifying the files yourself like that: `./GladosGenerator -l="./myLexicalRules" -s="./mySyntaxRules"`. Or you can just call `./GladosGenerator` which will use the default rules files provided at the root of the project.

The Compiler will be outputted in the `./Output` folder.

You should use the default rules files as template to make your own. For technical reason, the following tokens must be 1 char length:

```
EXPRESSION_LEFT_DELIMITER   ::= \(
EXPRESSION_RIGHT_DELIMITER  ::= \)
ARRAY_LEFT_DELIMITER        ::= \[
ARRAY_RIGHT_DELIMITER       ::= \]
TUPLE_LEFT_DELIMITER        ::= \{
TUPLE_RIGHT_DELIMITER       ::= \}
```

## B. Compiler testing

To build the compiler use the Makefile located at the root of the compiler source code. Use `make` or `make re`.

You can run the compiler using the examples files provided. All of thoses are written according to the default synthax but you can modify them to match your new grammar.

I recommand you running using this kind of command: `./glados -c examples/eq.csm test.pdp; ./glados -r test.pdp; cat examples-expected/eq.txt;`. This example will output the pseudo-code of eq.csm compiled, the result of the interpreted pseudo code and the expected result.

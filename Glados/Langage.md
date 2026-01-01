# PDP Language Documentation

## Types

PDP is a typed language. The type of a value must be specified whenever it is not obvious.

Here is the list of types supported by the language:

- `int`: represents a signed integer
    - `uint`: represents a positive integer
    - `char`: represents an integer between 0 and 255

The default base is decimal. However, a different base (between 2 and 36) can be specified with a prefix. Here is the list of supported prefixes:

| Prefix | Base |
| ------ | ---- |
| `Ob`   | 2    |
| `Oo`   | 8    |
| `Od`   | 10   |
| `Ox`   | 16   |
| `O{n}` | n    |

Each ASCII character, written between `'`, is considered an integer of type `char` whose value corresponds to its position in the ASCII table. For example, `'a'` is equal to `97`.

- `float`: represents a floating-point number (the decimal separator is the dot `.`)
- `bool`: represents a boolean value (`#t` = true; `#f` = false)
- `{a, b}`: represents a tuple of two values (the two values may have different types). `a` and `b` are template types.
- `[a]`: represents a list of values of the same type (a list is initialized with square brackets `[]`, and each element must be separated by a comma `,`). `a` is a template type.
    - `string`: an alias of `[char]`, represents a character string (a string is initialized with double quotes `""`)
- `procedure`: represents an invocable symbol (variable, function, etc.), and must be created at the root level (nested procedures are forbidden)
    - `<(parameters_types...) => return_type>`: represents a function or a lambda. `parameter_type` and `return_type` must be valid PDP types. There can be 0, 1, or several `parameter_type`. For example, this is a valid form: `<({a, integer} [a]) => a>`. *(A type is not a procedure. A keyword is not a procedure either.)*
- `type`: represents a type or a combination of types.

The type of a variable is strict. To transfer a value from one type to another, the builtin `cast` must be used. *For more information about `cast`, see the list of builtins.*

There is a special value compatible with all types: `NULL`. It is used to represent an absence of value, an erroneous value, etc.

It is possible to specify that a value may have several types using the `|` character. For example, for a value that can be `int` or `float`, the type should be `int|float`.

Several mixed types have predefined aliases:

| Alias   | Type combination |
| ------- | ---------------- |
| integer | `int\|uint\|char` |
| number  | `integer\|float` |
| any     | `number\|bool\|{a, b}\|[a]\|procedure\|type` |

## Syntax

The language syntax is based on parentheses, similar to LISP. There are several important rules to note:

- Whitespaces are interchangeable. For example, the following `define` expressions are all equivalent.

```lisp
(define name value)
```

```lisp
(
    define
    name
    value
)
```

```lisp
( define    name    value )
```

- Two symbols (variables, functions, etc.) cannot share the same name. This includes names reserved by builtins, variables, constants, and types defined in this documentation.
- A symbol name cannot contain whitespace or any of the following characters: `,`, `;`, `"`, `'`, `(`, `)`, `[`, `]`, `-`, `|`, `<`, `>`, `=`, `&`, `!`, `:`, `/`, `%`, `*`, `+`, `~`, `^`.

### Variable

*`define` and `=` are keywords.*

```lisp
(define name type value)
```

There are two predefined variables: `argc` & `argv`. These variables correspond to the arguments passed to the program.

`argc` is an `int` representing the number of arguments. If no arguments are provided, `argc` equals `0`.

`argv` is a `[string]` representing the list of arguments passed to the program. If no arguments are provided, `argv` equals `[]`.

### Function Definition

*`function` is a keyword.*

``lisp
(function name (parameter::type ...) (body...) return_type)
```

Example :

```lisp
(function f (a::int|float b::int|float) (+ a b) int|float)
```

There are predefined functions, essentially builtins. These are described later in this document.

### Function Call

```lisp
(name arguments...)
```

The type of each argument passed to a function must be identical to or more specific than the type of the corresponding parameter. For example:

```lisp
(function f (a::int|float b::int|float) (+ a b) int|float)
(f 1 0.1)
```

PDP supports **recursion**. For example:

```lisp
(function factorial (x::integer)
    (if (eq? x 1)
        1
        (* x (factorial (- x 1)))
    )
)
```

PDP supports **currying**. For example:

```lisp
(function f (a::int|float b::int|float) (+ a b) int|float)
((f 1) 0.1)
```

### Lambda

*`lambda` is a keyword.*

```lisp
((lambda (parameter::type ...) (body...) return_type) arguments...)
```

A lambda can be assigned to a variable. The result behaves similarly to using `function`. For example, the following two lines are equivalent:

```lisp
(function f (a::int|float b::int|float) (+ a b) int|float)
```

```lisp
(define f (lambda (a::int|float b::int|float) (+ a b) int|float))
```

### Condition

*`if` is a keyword.*

```lisp
(if (condition) (vraie) (fausse))
```

The *condition* must be an expression of type `bool`.

### Keywords

A "keyword" in PDP is a "function" that is processed (at least partially) during parsing rather than at compilation or runtime. A keyword may not return a value.

| Operator | Prototype | Action |
| --------- | --------- | ------ |
| `=` | `define (var::procedure t::type value::any)` | Assigns a value/expression of type `t` to a symbol named `var`. If the action fails without raising an error, `var` will be set to `NULL`. `var` can be a previously defined symbol or not. |
|     | `import (lib::string)`            | Adds the definitions from the library `lib` to the current file. NOTE: `import` must be alone on its line; otherwise, an error will be returned. |

### Builtins

#### Numeric Operators

| Operator | Prototype | Action |
| --------- | --------- | ------ |
| `+`   | `add (a::number b::number) number` | Performs $a + b$. |
| `-`   | `sub (a::number b::number) number` | Performs $a - b$. |
| `*`   | `mul (a::number b::number) number` | Performs $a \times b$. |
| `/`   | `div (a::number b::number) number` | Performs $a / b$. Returns `NULL` if $b=0$. |
| `%`   | `mod (a::integer b::integer) integer` | Performs $a \bmod b$. Returns `NULL` if $b=0$. |
| `**`  | `pow (a::number b::number) number` | Performs $a^b$. |
| `v-`  | `sqrt (a::number) float`           | Performs $\sqrt{a}$. Returns `NULL` if $a<0$. |
| `!!`  | `factorial (a::integer) uint`      | Performs $a!$. Returns `NULL` if $a<0$. |
| `+=`  | `add= (a::string b::number) bool`  | Applies `add` to `a` and `b`, storing the result in `a`. Returns `#t` if successful, `#f` otherwise. |
| `-=`  | `sub= (a::string b::number) bool`  | Applies `sub` to `a` and `b`, storing the result in `a`. Returns `#t` if successful, `#f` otherwise. |
| `*=`  | `mul= (a::string b::number) bool`  | Applies `mul` to `a` and `b`, storing the result in `a`. Returns `#t` if successful, `#f` otherwise. |
| `/=`  | `div= (a::string b::number) bool`  | Applies `div` to `a` and `b`, storing the result in `a`. Returns `#t` if successful, `#f` otherwise. |
| `%=`  | `mod= (a::string b::number) bool`  | Applies `mod` to `a` and `b`, storing the result in `a`. Returns `#t` if successful, `#f` otherwise. |
| `**=` | `pow= (a::string b::number) bool`  | Applies `pow` to `a` and `b`, storing the result in `a`. Returns `#t` if successful, `#f` otherwise. |

#### Boolean Operators

| Operator | Prototype | Action |
| --------- | --------- | ------ |
| `==`   | `eq (a::any b::any) bool`         | Performs $a = b$. |
| `!=`   | `neq (a::any b::any) bool`        | Performs $a \neq b$. |
| `<`    | `lw (a::number b::number) bool`   | Performs $a < b$. |
| `>`    | `gt (a::number b::number) bool`   | Performs $a > b$. |
| `<=`   | `lweq (a::number b::number) bool` | Performs $a \le b$. |
| `>=`   | `gteq (a::number b::number) bool` | Performs $a \ge b$. |
| `!`    | `not (a::bool) bool`              | Returns the opposite of `a`. |
| `&&`   | `and (a::bool b::bool) bool`      | Performs $a$ AND $b$. |
| `\|\|` | `or (a::bool b::bool) bool`       | Performs $a$ OR $b$. |
| `!&`   | `nand (a::bool b::bool) bool`     | Performs $a$ NAND $b$. |
| `!\|`  | `nor (a::bool b::bool) bool`      | Performs $a$ NOR $b$. |
| `:\|`  | `xor (a::bool b::bool) bool`      | Performs $a$ XOR $b$. |
| `!:`   | `xnor (a::bool b::bool) bool`     | Performs $a$ XNOR $b$. |

#### Binary Operators (on integers)

| Operator | Prototype | Action |
| --------- | --------- | ------ |
| `&`  | `band (a::integer b::integer) integer`   | Performs $a$ AND $b$. |
| `\|` | `bor (a::integer b::integer) integer`    | Performs $a$ OR $b$. |
| `~`  | `bnot (a::integer b::integer) integer`   | Performs bitwise NOT on $a$. |
| `^`  | `bxor (a::integer b::integer) integer`   | Performs $a$ XOR $b$. |
| `<<` | `lshift (a::integer b::integer) integer` | Performs a left shift of $a$ by $b$ positions. |
| `>>` | `rshift (a::integer b::integer) integer` | Performs a right shift of $a$ by $b$ positions. |

#### Tuple Library

| Prototype | Action |
| --------- | ------ |
| `left (t::{a, b}) a`      | Returns the left member of `t`. |
| `right (t::{a, b}) b`     | Returns the right member of `t`. |
| `swap (t::{a, b}) {b, a}` | Swaps the left and right members of `t`. |

#### List Library

| Prototype | Action |
| --------- | ------ |
| `len (l::[a]) uint`                       | Returns the length of `l`. |
| `concat (la::[a] lb::[a]) [a]`            | Concatenates two lists `la` and `lb`. |
| `find (l::[a] predica::{(a) => bool}) a`  | Applies `predica` to each element of `l` and returns the first element where `predica` returns `#t`. |
| `split (l::[a] i::uint) {[a], [a]}`       | Splits the list `l` into two lists at the i-th element. The i-th element becomes the last element of the left list. |
| `first (l::[a]) a`                        | Returns the first element of `l`. Returns `NULL` if `l` is empty. |
| `last (l::[a]) a`                         | Returns the last element of `l`. Returns `NULL` if `l` is empty. |
| `pushback (l::[a] item::a) [a]`           | Adds `item` to the end of `l`. |
| `pushfront (l::[a] item::a) [a]`          | Adds `item` to the beginning of `l`. |
| `get (l::[a] i::uint) a`                  | Returns the i-th element of `l`. |
| `reverse (l::[a]) [a]`                    | Reverses the order of elements in `l`. |

#### Math Library

| Constant | Value |
| --------- | ------ |
| `pi` | `3.14159265` |
| `e`  | `2.71828182` |

| Prototype | Action |
| --------- | ------ |
| `exp (a::number) float`           | Computes $e^a$. |
| `ln (a::number) float`            | Computes $ln(a)$. |
| `max (a::number b::number) number` | Returns the larger of `a` and `b`. |
| `min (a::number b::number) number` | Returns the smaller of `a` and `b`. |
| `cos (a::number) float`            | Computes $cos(a)$. |
| `acos (a::number) float`           | Computes $acos(a)$. |
| `cosh (a::number) float`           | Computes $cosh(a)$. |
| `sin (a::number) float`            | Computes $sin(a)$. |
| `asin (a::number) float`           | Computes $asin(a)$. |
| `sinh (a::number) float`           | Computes $sinh(a)$. |
| `tan (a::number) float`            | Computes $tan(a)$. |
| `atan (a::number) float`           | Computes $atan(a)$. |
| `tanh (a::number) float`           | Computes $tanh(a)$. |
| `ceil (a::float) float`            | Rounds `a` up. |
| `round (a::float) float`           | Rounds `a` to the nearest integer. |
| `trunc (a::float) float`           | Truncates `a` to an integer. |
| `floor (a::float) float`           | Rounds `a` down. |

## Comments in Code

You can add comments in the code using the following syntax: `--`.

A comment starts with `--` and ends either:
- at the end of the line, or
- when the `--` syntax appears again on the same line.

## Example

```
-- This is a comment; it will not be executed --

This is not a comment

-- This is a comment, but this is code: -- (+ 4 5)
```

### Explanation:

1. The first comment is ignored by the interpreter.
2. The second line is invalid code.
3. The third line contains:
   - a comment, which starts and ends with `--`.
   - a functional expression `( + 4 5 )`, which is interpreted as code.

Use this feature to clarify your code without affecting its execution.

# Arthon Programming Language

## Overview

Arthon is a small, educational programming language implemented in Python using the lark parsing library. It is designed to explore how programming languages work, such as, parsing, interpretation, scoping, closures, and static type checking.

This project was built as a learning exercise.

## Features

- Integer, float, boolean, and none values
- Variable declaration and assignment 
- Arithmetic and comparison operators
- `if`, `while`, and `for` control flow
- Block scoping using `{...}`
- Functions with parameters and return values
- Lambda expressions and closures
- `print` statement
- Comments using `:`

## Language Example

```
var x = 5; : creating a variable named x with the value of 5.

: creating an adding function.
func add(a, b) {
    return a + b;
}

: printing out the expression.
print(add(x, 5));
```

Output:

> 10

## Grammar Overview

Arthon programs consist of statements, which can be:

- Variable declaration (`var x = 6;`)
- Variable assignment (`x = 10`)
- Epressions (`x + x`)
- Control flow (`if`, `while`, `for`)
- Functions (`func x() {}`)
- Blocks (`{ :some statement here }`)

## Interpreter Architecture

### Environment

- `Env` implements lexical scoping.
- Variables are resolved by following the environment nesting.

### Closures

- Functions are represented as `Closure` objects
- A `Closure` consists of:
  - Parameter list
  - Function body
  - Environment object

### Returns

- `return` is implemented using a custom `Return` exception class.
- This will cleanly exit deeply nested statements.

## Type Checker

The type checker is implemented separately from the interpreter and includes:

### Supported Types

- `Int`
- `Bool`
- `Char`
- `None`
- Function types

### Type Rules

- Arithmetic operators require `Int`
- Comparison operators return `Bool`
- Variables cannot change type after declaration
- Function calls must match parameter count and types

> Note: Currently, all function parameters are assumed to be Int, this will hopefully be fixed later on.

## Running the Language

### Requirements

- Python 3.10+
- lark python library

To intall the dependencies:

```bash
pip install lark
```

### Run a Program

Linux:

```bash
python3 arthon.py < program.art
```

Windows:

```bash
type .\program.art | python .\arthon.py
```

> Note: while the example shows 'program.art', truth is, you can use any extension to the file (or no extension at all), as long as it's in the format the program can read.

## Future Work

- Properly implement the Type Checker. Works but not that great.
- Improve the grammar of the programming language.
- Add custom error classes w\ information about where the error occurs.

## Author

Developed by **Artem** as an educational project exploring programming language design and implementation.

## License

This project is licensed under the **Apache 2.0 license.**
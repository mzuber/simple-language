The SL Programming Language
===========================

SL ( _Simple Language_ ) is a small, statically-typed, purely functional programming language.
Its keyword-heavy style to structure definitions and expressions is inspired by languages
like Opal, its syntax for expressions and data type definitions is close to Haskell's one.

This repositroy provides a front-end implementation for SL which can be used as a starting
point for research and teaching purposes in the field of programming languages and compiler
construction.

### SL Syntax
An SL program consists of a sequence of data type definitions, function signatures, and
function definitions that may appear in any order.

###### Data Type and Function Definitions
A data type definition introduces a type name and one or more data constructors. Data types
may be recursive and parametric. We can define the type of polymorphic lists with the following
piece of code:
```
DATA List a = Cons a (List a) | Nil
```
In addition to the types defined by a program's data type definitions, SL has predefined types
for integers (Int), characters (Char), and strings (String) respectively. Common functions on
these types are defined as built-ins.

Top-level function definitions are pattern based and consist of one or more clauses. The clauses
of a function are tried in top-down order until the first matching pattern is found (first-fit
pattern matching). As an example, we define the well-known `map` function on lists:
```
DEF map f Nil         = Nil
DEF map f (Cons x xs) = Cons (f x) (map f xs)
```
In addition to regular function definitions the programmer can define custom binary operators.
An operator definition is a regular function definition where the operator name is stated infix.

Note that we do not have to write down types for functions, the SL front-end is able to infer the
most general type for each function — if it is type correct at all. Nevertheless, the programmer
can still provide a type signature for each top-level function definition, i.e., to specialize a
function according to her own choice. Given the `map` function defined earlier, the programer can
explicitly give a typing for this function by providing a corresponding signature for the function
definitions:
```
FUN map : (a -> b) -> List a -> List b
```

###### Expressions
On the expression level SL provides lambda-abstractions, application of functions, conditionals,
and local definitions.

Function application of a function _f_ to an argument _a_ is written as juxtaposition of _f_ and _a_
without parentheses: `f a`. A lambda-abstraction introduces an anonymous function. Like in top-level
definitions, pattern matching is used for the arguments.

SL has two kinds of conditionals: if- and case-expressions. The condition in an if-expression must be
of type `Bool` (defined in the SL prelude). Case-expressions perform pattern-matching for a single
expression, i.e., we can write a `length` function for lists in a single clause using a case-expression:
```
DEF length l = CASE l
                 OF Nil       THEN 0
                 OF Cons x xs THEN 1 + (length xs)
```
Similar to pattern-based top-level definitions, pattern matching uses a top-down first-fit strategy.

An expression may contain local definitions using a let-expression, e.\,g.
```
LET
  even = \ n. IF n==0 THEN True ELSE odd (n-1)
  odd  = \ n. IF n==1 THEN True ELSE even (n-1)
IN even 5
```
The names introduced in a let-expression may be used in the right-hand sides, even mutually recursive.
There is, however, an important restriction due to SL's eager evaluation strategy: In a set of mutually
recursive definitions, all right-hand sides must be lambda-expressions.

### Acknowledgements

The first version of the SL language as well as a corresponding Haskell front-end was developed by
Andreas Büchele, Florian Lorenzen, and Judith Rohloff. The Scala implementation of the SL compiler
in this repository was developed by Christoph Höger, Fabian Linges, and Martin Zuber.

# Compiler
Project implementing a compiler in OCaml
for CSC 312 at Grinnell college.

Implemented by Reilly Noonan Grant

## Project Overview
This project is the begining of a compiler written in OCaml. It is
being created primarily for educational purposes.

Currently, this project implements a parser, and lexer for
support for a interpreter of a programing language based on
OCaml.

## Setup Instructionse
Ensure that OCaml is installed

and ensure that
\#use "topfind";;
\#camlp4o;;
\#thread;;
\#require "core.top";;
\#require "core.syntax";;
are in you .ocamlinit file

## Build Instructions

You can build the project with make

## Run and Test
After making the project, you can run it by using compiler.native on a
formated file. Examples of such files are provided in the tests
directory with the extension .arith. Example test files

#### test.arith
1 + 2

#### test.out
3

#### test.parse.out
( + 1 2 )

where test.arith is the input, and test.out is the expected output,
and test.parse.out is the expected output when the parse flag is
enabled.

You can also run tests using make test, or running the run_test.sh script

## Flags
We currently support the -parse flag, which will display the
result of parsing the code in lisp syntax,  the -step flag
which will evaluate the code incrementally until a value is reached,
and the -typecheck flag which evaluates the type of the program.

## Language description
This project currently supports evaluation of a
 language with the following grammar

e ::= n | f | (e) | e1 + e2 | e1 - e2 | e1 * e2 | e1 /e2 
..    | true | false | e1 <= e2 |e1 < e2|e1 = e2||e1 > e2 |e1 >= e2
..    | if e1 then e2 else e3 | NaN | e1 and e2 | e1 or e2
..    | let x : t = e1 in e2 | fun (x:t1) :t2 = e
..    | fix f (y :t1) : t2 = e
..    | e1 e2 |() | (e1,e2,..,en) | fst e | snd e| nth n e
..    |[] : t | e1 :: e2 | hd e | tl e | empty e
..    | ref e | e1 := e2 | !e | e1 ; e2 | while e1 do e2 end


t ::= int | bool | t1 -> t2 | () | t1 * t2* .. *tn | <t>

## Changelog

Build on 03/16/18 First:
### New features

- Replaced Pairs with Tuples, a more general version
- Added the "nth" unary operator
- Updated types to reflect this

### changes

- Implemented above features

### Known Bugs

- Do not support nested variables with the same name
- Does not currently support currying



Build on 03/16/18 First:
### New features

- Updated Grammar in readme
- Added state:
--Ref creates a ref cell, which can be derefrenced with
.. ! to get the value assigned
-- := assigns a ref cell to have a new value of the same type
-- ; allows for sequences of expression, by evaluating the first
.. expression, then evaluating the second and returning it
-- Added while loops
- Updated tests to support new features


### changes

- Implemented above features

### Known Bugs

- Do not support nested variables with the same name
- Does not currently support currying



Build on 03/15/18:
### New features

- Implemented Typechecking
- Added Pairs, Lists, and the Unit type
- Added fst, snd for getting the first and second element of a pair
- Added hd and tl for getting the first element of a list, and getting
.. all but the first element of the list
- Added empty for checking a list to see if it is empty


### changes

- Implemented above features
- 

### Known Bugs

- Do not support nested variables with the same name
- Does not currently support currying


Build on 02/25/18:
### New features

- Added flag to the compiler that allows incremental evaluation


### changes

- Implemented above feature
- Changed evaluation to use the step by step evalution

### Known Bugs

- Do not support nested variables with the same name



Build on 02/23/18:
### New features

- Added Let binding,
- Added non recursive functions
- Added recursive functions
- Added support for entering NaN directly to in a source file
- Added support for entering Booleans directly to in a source file
- Added support for boolean operators and and or
- Added support for numerical comparisons other than <=


### changes

- Added above features
- Implemented the subst function to facilitate let bindings and
  functions
- Added new tests to support new features

### Known Bugs

- Do not support nested variables with the same name



Build on 02/20/18:
### New features

- Switched to an infix notation

### changes

-  Changed implementation to use a lexer and
.. parser generator
- Switched to infix notation

### Known Bugs

- No known bugs


Build on 02/07/18:
### New features

- Added support for floats, and NaN

### changes

- Added support for floats, and NaN

### Known Bugs

- No known bugs

Build on 02/06/18:
### New features

- Created a basic Command line interface.
- Takes arguments at the command line and returns them
- Given the -length flag returns the lengths of each argument
-  Added ability to lex, parse and evaluate expressions given in file 
.. format
- Support for addition, multiplication, subtraction and division
- Support for booleans
- Support for the less than or equal operation
- Support for if expressions
- Catches type errors, and identifies the expression at fault
- Complete code overhaul, based partially on
.. https://github.com/psosera/csc312-example-compiler

### Known Bugs

- No known bugs


Build on 01/29/18:
### New features

- Created a basic Command line interface.
- Takes arguments at the command line and returns them
- Given the -length flag returns the lengths of each argument

### Changes

- First Build

### Known Bugs

- No known bugs

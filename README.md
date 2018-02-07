# Compiler
Project implementing a compiler in OCaml
for CSC 312 at Grinnell college.

Implemented by Reilly Noonan Grant

## Project Overview
This project is the begining of a compiler written in OCaml. It is
being created primarily for educational purposes.

Currently, this project implements basic a command line interface.

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

You can build the project with
make

## Run and Test
After making the project, you can run it by using compiler.native on a
formated file. Examples of such files are provided in the tests
directory. You can also run tests using make test. Example test

## Language description
This project currently supports evaluation of a a language with the following grammar

e ::= n | (+ e1 e2) | (- e1 e2) | (* e1 e2) | (/ e1 e2)
..    | true | false | (<= e1 e2) | (if e1 e2 e3)


Build on 02/06/18:
### New features

- Created a basic Command line interface.
- Takes arguments at the command line and returns them
- Given the -length flag returns the lengths of each argument

### Changes

-  Added ability to lex, parse and evaluate expressions given in file 
.. format
- Support for addition, multiplication, subtraction and division
- Support for booleans
- Support for the less than or equal operation
- Support for if expressions
- Catches type errors, and identifies the expression at fault


### Changes

- Complete code overhaul, based partially on
.. https://github.com/psosera/csc312-example-compiler

### Known Bugs

- No known bugs

## Changelog

Build on 01/29/18:
### New features

- Created a basic Command line interface.
- Takes arguments at the command line and returns them
- Given the -length flag returns the lengths of each argument

### Changes

- First Build

### Known Bugs

- No known bugs

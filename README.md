# Compiler
Project implementing a compiler in OCaml
for CSC 312 at Grinnell college.

Implemented by Reilly Noonan Grant

## Project Overview
This project is the begining of a compiler written in OCaml. It is
being created primarily for educational purposes.

Currently, this project implements basic a command line interface.

## Setup Instructionse
Ensure that the Core package is installed. If you have opam
installed,run
opam install core
opam install async
opam install core_extended

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
After making the project, you can run it using main.native. You can
also run tests using make test


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

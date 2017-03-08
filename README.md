TSVG
===============

## Description
Represents a parser and an interpreter to create SVG files from a simple imperative code. 
The parser creates an abstract syntax tree from a sequence of characters. 
Afterwards, the interpreter evaluates the system state and executes the commands.

## Prerequisites

+ GHC
+ GHCI

## Usage
At first, clone or download this project. Afterwards, run your console and compile respectively start the project with the following commands:
```
cd xmas
make
...
./xmas "your_file.tsvg"
```
The TSVG language supports variables, loops, conditional instructions, arithmetic operations and many other commands, for example:
```
tsvg example2 (500, 500);

var cnt;
var alpha;

cnt := 0;
alpha := 360 / 27;

while cnt <= 27 {  
  slice (200, alpha* cnt, alpha*(cnt+ 1));
  colour next;
  cnt := cnt+1
}
```
This code creates a colorful circle.

## More information
Read more about Haskell at https://www.haskell.org/documentation. Moreover, there are many comments about this program in the source files.

# Subleq
Subleq interpreter in Fortran

![Screenshot](https://github.com/James-P-D/Subleq/blob/main/screenshot.gif)

## Introduction

(More information can be found on the [Esolang website](https://esolangs.org/wiki/Subleq), which is also from where I stole my various example programs.)

Subleq is a simple [One Instruction Set Computer](https://en.wikipedia.org/wiki/One-instruction_set_computer) (OISC). An OISC is a theoretical abstract machine which has only a single assembly langauge opcode (unlike, for example, x86 Asm which has `mov`, `lea`, `cmp`, `jnz`, and dozens of other possible instructions.) The only instruciont in Subleq is SUBtract if Less-than or EQual, from which we get it's name.

## Tutorial

Since the language only has one op-code, the op-code itself it typically omitted and each instruction is simply written as three separate arguments, separated by spaces, with each instruction on a new line:

```
A B C
```

For each instruction, we subtract the value of the memory address A from the memory address B, store the result in memory address B, and if the result of the subtraction is less than or equal to zero, execution jumps to memory address C.

To make programming in Subleq *slightly* easier, we can substitute parameters with variable names, with the identifier followed by a `:` character and the value of the memory location. For example, the following Subleq program..

```
X Y 6
X:7 Y:7 7
X Y 0
```

..can be resolved to:

```
3 4 6
7 7 7
3 4 0
```

We can also use `?` to specify relative memory locations. For example...

```
?+5 ?+0 ?-5
```

...can be resolved to:

```
5 1 -3
```

Finally, we can add comments to Subleq programs by using the `#` symbol.

## Hello, World!

Below is an example 'Hello, world!' program:

```
# Output the character pointed to by p.
a a ?+1
p Z ?+1
Z a ?+1
Z Z ?+1
a:0 -1 ?+1

# Increment p.
m1 p ?+1

# Check if p < E.
a a ?+1
E Z ?+1
Z a ?+1
Z Z ?+1
p a -1

Z Z 0

p:H Z:0 m1:-1

# Our text "Hello, world!\n" in ASCII codes
H:72 101 108
108 111 44
32 87 111
114 108 100
33 10 E:E
```

Which in resolved syntax is:

```
12 12 3
36 37 6
37 12 9
37 37 12
0 -1 15
38 36 18
12 12 21
53 37 24
37 12 27
37 37 30
36 12 -1
37 37 0
39 0 -1
72 101 108
108 111 44
32 87 111
114 108 100
33 10 53
```

## Compiling

The program was written in Fortran90 using gfortran running on CygWin64.

To compile:

```
gfortran subleq.f90 -o subleq.exe
```

The program expects a single parameter specifying a Subleq file. To run our 'hello world' example we would use:

```
./subleq.exe hello_world.slq
```

## Known Bugs

This is my first (and probably last) attempt at using Fortran. 

The program makes no attempt at detecting circular-references. The following program will cause a stack-overflow crash since it's impossible to resolve the three references:

```
a:b b:c c:a
```
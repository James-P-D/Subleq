# Subleq
Subleq interpreter in Fortran

![Screenshot](https://github.com/James-P-D/Subleq/blob/main/screenshot.gif)

## Introduction

Subleq is a simple (One Instruction Set Computer](https://en.wikipedia.org/wiki/One-instruction_set_computer) (OISC). An OISC is a theoretical abstract machine which has only a single assembly langauge opcode (unlike, for example, x86 Asm which has `mov`, `lea`, `cmp`, `jnz`, and several other possible instructions.) The only instruciont in Subleq is SUBtract if Less-than or EQual, from which we get it's name.

More information can be found on the [Esolang website](https://esolangs.org/wiki/Subleq), which is also from where I stole my various example programs.


```
H -1 3
i -1 6
0 0 -1
H:72 i:105 0
```

```
9 -1 3
10 -1 6
0 0 -1
72 105 0
```

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
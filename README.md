# PushPop [![Build Status](https://travis-ci.org/michajlo/pushpop.png)](https://travis-ci.org/michajlo/pushpop)

A simple stack based language/VM built out of curiosity as a means of
exploration.

This all grew from a desire to learn about tail call optimization via
implementation. To make it count I felt like I needed to target some
assembly language, but that would require learning the conventions of
an existing stack. That seemed like a lot of work, so I figured I'd
learn by creating my own.

PushPop is largely split into two parts- the low level VM bits and
the higher level language compiler, more on those below.

Some of the terminology may be a bit off from the academic norms, I've
tried to avoid consulting the existing literature as part of the fun.


# Virtual Machine

At the core of PushPop is a simple virtual machine which maintains two
stacks- a data stack and an instruction pointer stack. The data stack
may contain any type of data, the instruction stack in theory can hold
any type of data, but in practice only contains integer pointers to
locations in code. The virtual machine state is manipulated by a series
of instructions defined by the PushPop instruction set.


## Instruction Set

The PushPop instruction set consists of a minimal collection of stack
manipulation and control flow operations.  It can be found in the "Asm"
source file.


## Assembly Language

A basic assembly interpreter/compiler is provided.  Assembly files are
parsed and compiled into a proper in-memory representation of the
program with all labels filled in.  See the test.asm file in the root
for an example.


# Language

Because most sane people hate writing assembly, and my inspiration was
to implement tail call optimizations, a higher level language exists.
The syntax is exceptionally primitive and not heavily tested- it's just
enough to build up an AST where all the fun stuff happens.

Until I get the motivation to write more documentation, check out the
.pp files in the root.


## License

Apache 2.0

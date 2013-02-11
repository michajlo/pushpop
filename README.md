# PushPop [![Build Status](https://travis-ci.org/michajlo/pushpop.png)](https://travis-ci.org/michajlo/pushpop)

A simple stack based language/VM built out of curiosity as a means of
exploration.

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


## License

Apache 2.0

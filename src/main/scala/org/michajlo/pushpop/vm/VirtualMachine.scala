package org.michajlo.pushpop.vm

import scala.annotation.tailrec

import Asm.Add
import Asm.CallBIF
import Asm.Div
import Asm.Insn
import Asm.Jsr
import Asm.Mul
import Asm.Pop
import Asm.Push
import Asm.Ret
import Asm.Sub
import Asm.Nop


/**
 * Executes Asm.Insns on an internally maintained stack,
 * this guy is the heart of the execution
 */
class VirtualMachine {

  val dataStack: Stack = new Stack
  val insnPtrStack: Stack = new Stack

  /**
   * Execute a series of instructions.
   *
   * The stack is left in the state in which the program
   * leaves it at the end of execution
   *
   * @param program list of instructions to execute
   */
  def run(program: List[Insn], insnPtr: Int = 0) {
    execute(program.toArray, insnPtr)
  }

  /**
   * Execute insns starting at insnPtr
   *
   * @param insns instructions to execute
   * @param insnPtr instruction pointer to run from (default 0)
   */
  @tailrec
  final def execute(insns: Array[Insn], insnPtr: Int = 0) {
    if (insnPtr < insns.length) {
      insns(insnPtr) match {
        case Push(value) =>
          dataStack.push(value)
          execute(insns, insnPtr + 1)
        case Pop =>
          dataStack.pop()
          execute(insns, insnPtr + 1)

        case Nop => execute(insns, insnPtr + 1)

        case CallBIF => dataStack.pop() match {
          case "print" =>
            System.out.println(dataStack.pop())
            execute(insns, insnPtr + 1)
          case "exit" =>
        }

        case Add => (dataStack.pop(), dataStack.pop()) match {
          case (a1: Int, a2: Int) =>
            dataStack.push(a1 + a2)
            execute(insns, insnPtr + 1)
        }

        case Sub => (dataStack.pop(), dataStack.pop()) match {
          case (r: Int, l: Int) =>
            dataStack.push(l - r)
            execute(insns, insnPtr + 1)
        }

        case Mul => (dataStack.pop(), dataStack.pop()) match {
          case (m1: Int, m2: Int) =>
            dataStack.push(m1 * m2)
            execute(insns, insnPtr + 1)
        }

        case Div => (dataStack.pop(), dataStack.pop()) match {
          case (r: Int, l: Int) =>
            dataStack.push(l / r)
            execute(insns, insnPtr + 1)
        }

        case Jsr(newInsnPtr) =>
          insnPtrStack.push(insnPtr + 1)
          execute(insns, newInsnPtr)

        case Ret => insnPtrStack.pop() match {
          case newInsnPtr: Int => execute(insns, newInsnPtr)
        }
      }
    }
  }
}
package org.michajlo.pushpop.vm

import scala.annotation.tailrec

import Asm.Add
import Asm.Assign
import Asm.CallBIF
import Asm.Div
import Asm.Insn
import Asm.Mul
import Asm.Pop
import Asm.Push
import Asm.Sub
import Asm.Jsr
import Asm.Ret


/**
 * Executes Asm.Insns on an internally maintained stack,
 * this guy is the heart of the execution
 */
class VirtualMachine {

  val stack: Stack = new Stack

  /**
   * Execute a series of instructions.
   *
   * The stack is left in the state in which the program
   * leaves it at the end of execution
   *
   * @param program list of instructions to execute
   */
  def run(program: List[Insn]) {
    execute(program.toArray)
  }

  /**
   * Execute insns startin at insnPtr
   *
   * @param insns instructions to execute
   * @param insnPtr instruction pointer to run from (default 0)
   */
  @tailrec
  final def execute(insns: Array[Insn], insnPtr: Int = 0) {
    if (insnPtr < insns.length) {
      insns(insnPtr) match {
        case Push(value) =>
          stack.push(value)
          execute(insns, insnPtr + 1)
        case Pop =>
          stack.pop()
          execute(insns, insnPtr + 1)

        case CallBIF => stack.pop() match {
          case "print" =>
            System.out.println(stack.pop())
            execute(insns, insnPtr + 1)
          case "exit" =>
        }

        case Add => (stack.pop(), stack.pop()) match {
          case (a1: Int, a2: Int) =>
            stack.push(a1 + a2)
            execute(insns, insnPtr + 1)
        }

        case Sub => (stack.pop(), stack.pop()) match {
          case (r: Int, l: Int) =>
            stack.push(l - r)
            execute(insns, insnPtr + 1)
        }

        case Mul => (stack.pop(), stack.pop()) match {
          case (m1: Int, m2: Int) =>
            stack.push(m1 * m2)
            execute(insns, insnPtr + 1)
        }

        case Div => (stack.pop(), stack.pop()) match {
          case (r: Int, l: Int) =>
            stack.push(l / r)
            execute(insns, insnPtr + 1)
        }

        case Jsr(newInsnPtr) =>
          stack.push(insnPtr + 1)
          execute(insns, newInsnPtr)

        case Ret => stack.pop() match {
          case newInsnPtr: Int => execute(insns, newInsnPtr)
        }


        case Assign(offset, value) =>
          stack.assign(offset, value)
          execute(insns, insnPtr + 1)
      }
    }
  }
}
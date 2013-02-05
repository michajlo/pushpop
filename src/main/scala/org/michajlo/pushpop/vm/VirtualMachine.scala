package org.michajlo.pushpop.vm

import Asm._

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
    program.foreach {
      case Push(value) => stack.push(value)
      case Pop => stack.pop()

      case CallBIF => stack.pop() match {
        case "print" =>
          System.out.println(stack.pop())
      }

      case Add => (stack.pop(), stack.pop()) match {
        case (a1: Int, a2: Int) => stack.push(a1 + a2)
      }

      case Sub => (stack.pop(), stack.pop()) match {
        case (r: Int, l: Int) => stack.push(l - r)
      }

      case Mul => (stack.pop(), stack.pop()) match {
        case (m1: Int, m2: Int) => stack.push(m1 * m2)
      }

      case Div => (stack.pop(), stack.pop()) match {
        case (r: Int, l: Int) => stack.push(l / r)
      }

      case Assign(offset, value) =>
        stack.assign(offset, value)
    }
  }
}
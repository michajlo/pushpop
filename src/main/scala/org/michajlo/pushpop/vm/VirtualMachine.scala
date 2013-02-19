package org.michajlo.pushpop.vm

import scala.annotation.tailrec

import Asm._

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

        case Add =>
          val (r, l) = popTwoInts()
          dataStack.push(l + r)
          execute(insns, insnPtr + 1)

        case Sub =>
          val (r, l) = popTwoInts()
          dataStack.push(l - r)
          execute(insns, insnPtr + 1)

        case Mul =>
          val (r, l) = popTwoInts()
          dataStack.push(l * r)
          execute(insns, insnPtr + 1)

        case Div =>
          val (r, l) = popTwoInts()
          dataStack.push(l / r)
          execute(insns, insnPtr + 1)

        case CmpGt =>
          val (r, l) = popTwoInts()
          dataStack.push(l > r)
          execute(insns, insnPtr + 1)

        case CmpGte =>
          val (r, l) = popTwoInts()
          dataStack.push(l >= r)
          execute(insns, insnPtr + 1)

        case CmpLt =>
          val (r, l) = popTwoInts()
          dataStack.push(l < r)
          execute(insns, insnPtr + 1)

        case CmpLte =>
          val (r, l) = popTwoInts()
          dataStack.push(l <= r)
          execute(insns, insnPtr + 1)

        case CmpEq =>
          val (r, l) = popTwoInts()
          dataStack.push(l == r)
          execute(insns, insnPtr + 1)

        case CmpNeq =>
          val (r, l) = popTwoInts()
          dataStack.push(l != r)
          execute(insns, insnPtr + 1)

        case Jsr(newInsnPtr) =>
          insnPtrStack.push(insnPtr + 1)
          execute(insns, newInsnPtr)

        case Ret => insnPtrStack.pop() match {
          case newInsnPtr: Int => execute(insns, newInsnPtr)
        }

        case Jmp(newInsnPtr) => execute(insns, newInsnPtr)

        case JmpT(newInsnPtr) =>
          execute(insns, if (dataStack.pop().asInstanceOf[Boolean]) newInsnPtr else insnPtr + 1)

        case JmpF(newInsnPtr) =>
          execute(insns, if (!dataStack.pop().asInstanceOf[Boolean]) newInsnPtr else insnPtr + 1)

        case JmpZ(newInsnPtr) =>
          execute(insns, if (dataStack.peek == 0) newInsnPtr else insnPtr + 1)

        case LPush(stackOff) =>
          dataStack.push(dataStack.get(stackOff))
          execute(insns, insnPtr + 1)

        case Assign(stackOff) =>
          dataStack.assign(stackOff, dataStack.pop())
          execute(insns, insnPtr + 1)
      }
    }
  }

  private def popTwoInts(): (Int, Int) = (dataStack.pop(), dataStack.pop()) match {
    case (r: Int, l: Int) => (r, l)
    case (fst, scnd) =>
      throw new IllegalStateException("Expected two ints on the stack, but got: " +
          fst + "(" + fst.getClass +") and " + scnd + "(" + scnd.getClass + ")" )
  }
}
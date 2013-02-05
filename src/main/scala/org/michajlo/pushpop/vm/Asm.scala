package org.michajlo.pushpop.vm

/**
 * The pushpop VM instruction set
 */
object Asm {

  /**
   * Top level trait encapsulating all instructions
   */
  trait Insn

  /**
   * Push a value onto the stack
   */
  case class Push(value: Any) extends Insn

  /**
   * Pop a value from the stack
   */
  case object Pop extends Insn

  /**
   * Call a built in function
   */
  case object CallBIF extends Insn

  /**
   * Add the top two items on the stack
   */
  case object Add extends Insn

  /**
   * Subtract the first item on the stack from the second
   */
  case object Sub extends Insn

  /**
   * Divide the second item in the stack by the second one
   */
  case object Div extends Insn

  /**
   * Multiply the top two items on the stack
   */
  case object Mul extends Insn

  /**
   * Assign value to offset from the current stack pointer
   */
  case class Assign(offset: Int, value: Any) extends Insn
}
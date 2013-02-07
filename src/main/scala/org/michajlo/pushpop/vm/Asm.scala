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
   * Does nothing
   */
  case object Nop extends Insn

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
   * Jump to subroutine located at newInsnPtr
   *
   * Pushes the next insnPtr onto the instruction pointer stack
   */
  case class Jsr(newInsnPtr: Int) extends Insn

  /**
   * Return from a subroutine, pops the stored insnPtr off the
   * instruction pointer stack and goes to it
   */
  case object Ret extends Insn

  /**
   * Unconditionally jump to newInsnPtr
   */
  case class Jmp(newInsnPtr: Int) extends Insn

  /**
   * Jump to newInsnPtr if the value on the top of the stack is zero
   *
   * Does not pop the value on the top of the stack, but this may change
   * pending patterns that emerge
   */
  case class JmpZ(newInsnPtr: Int) extends Insn

  /**
   * Load a value from an offset into the stack and push it on
   * top
   */
  case class LPush(stackOff: Int) extends Insn

  /**
   * Pop a value from the stack and assign it to stackOffset into the stack
   * after the pop
   */
  case class Assign(stackOff: Int) extends Insn
}
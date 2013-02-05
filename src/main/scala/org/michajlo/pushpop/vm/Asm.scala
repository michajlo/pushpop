package org.michajlo.pushpop.vm

object Asm {

  trait Insn

  case class Push(value: Any) extends Insn
  case object Pop extends Insn
  case object CallBIF extends Insn

  case object Add extends Insn
  case object Sub extends Insn
  case object Div extends Insn
  case object Mul extends Insn

  case class Assign(stackOff: Int, value: Any) extends Insn
}
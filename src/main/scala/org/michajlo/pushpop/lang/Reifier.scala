package org.michajlo.pushpop.lang

import org.michajlo.pushpop.vm.Asm

object Reifier {

  def reifyDeclare(declare: Ast.Declare, vars: List[String]): (List[Asm.Insn], List[String]) = {
    val insns = List(Asm.Push(declare.value.getOrElse(null)))
    val newVars = declare.ident :: vars
    (insns, newVars)
  }

  def reifyIdent(ident: Ast.Ident, vars: List[String]): (List[Asm.Insn], List[String]) = vars.indexOf(ident.ident) match {
    case -1 => throw new IllegalStateException("var " + ident + " undefined")
    case stackOffset =>
      val insns = List(Asm.LPush(stackOffset))
      (insns, vars)
  }

  def reifyConst(const: Ast.Const, vars: List[String]): (List[Asm.Insn], List[String]) = {
    val insns = List(Asm.Push(const.v))
    (insns, vars)
  }
}
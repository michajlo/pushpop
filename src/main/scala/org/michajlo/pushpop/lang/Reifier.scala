package org.michajlo.pushpop.lang

import org.michajlo.pushpop.vm.Asm

object Reifier {

  def reify(nodes: List[Ast.Node]): List[Asm.Insn] = reify(nodes, Nil, Nil)

  private def reify(nodes: List[Ast.Node], vars: List[String], accum: List[List[Asm.Insn]]): List[Asm.Insn] = nodes match {
    case Nil => accum.reverse.flatten
    case node :: rest =>
      val (insns, newVars) = reify(node, vars)
      reify(rest, newVars, insns :: accum)
  }

  def reify(node: Ast.Node, vars: List[String] = Nil): (List[Asm.Insn], List[String]) = node match {

    case Ast.Declare(name, None) =>
      (List(Asm.Push(null)), name :: vars)
    case Ast.Declare(name, Some(rhs)) =>
      (reify(rhs, vars)._1, name :: vars)

    case Ast.Ident(name) => vars.indexOf(name) match {
      case -1 => throw new IllegalStateException("var " + name + " undefined")
      case n =>
        (List(Asm.LPush(n)), vars)
    }

    case Ast.Const(value) =>
      (List(Asm.Push(value)), vars)

    case Ast.Add(lhs, rhs) =>
      val insns = reify(lhs, vars)._1 ++ reify(rhs, "#nil" :: vars)._1 ++ List(Asm.Add)
      (insns, vars)

    case Ast.Sub(lhs, rhs) =>
      val insns = reify(lhs, vars)._1 ++ reify(rhs, "#nil" :: vars)._1 ++ List(Asm.Sub)
      (insns, vars)
  }

}
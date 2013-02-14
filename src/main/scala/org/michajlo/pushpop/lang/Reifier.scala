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

    case Ast.Declare(name, rhs) =>
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

    case Ast.Mul(lhs, rhs) =>
      val insns = reify(lhs, vars)._1 ++ reify(rhs, "#nil" :: vars)._1 ++ List(Asm.Mul)
      (insns, vars)

    case Ast.Div(lhs, rhs) =>
      val insns = reify(lhs, vars)._1 ++ reify(rhs, "#nil" :: vars)._1 ++ List(Asm.Div)
      (insns, vars)

    case Ast.Block(stmts, result) =>
      val initVars = vars
      // accumulate instructions and vars
      val (insns, endVars) = stmts.foldLeft((List[List[Asm.Insn]](), initVars)) {
        case ((insnsSoFar, varsSoFar), decl: Ast.Declare) =>
          val (declInsns, newVars) = reify(decl, varsSoFar)
          (declInsns :: insnsSoFar, newVars)
        case ((insnsSoFar, varsSoFar), expr: Ast.Expr) =>
          // this is being reversed
          (List(Asm.Pop) :: reify(expr, varsSoFar)._1 :: insnsSoFar, varsSoFar)
      }

      // get instructions for final statement (has no effect on vars)
      val resultInsns = reify(result, endVars)._1

      // push value on top down while popping for return
      val resultPushInsns = List.range(0, endVars.size - initVars.size).map(_ => Asm.Assign(0))

      // put em together proper (we've built the insns up reversed so far...
      val finalInsns = (resultPushInsns :: resultInsns :: insns).reverse.flatten
      (finalInsns, initVars)
  }


}
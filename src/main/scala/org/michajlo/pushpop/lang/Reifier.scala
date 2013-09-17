package org.michajlo.pushpop.lang

import org.michajlo.pushpop.vm.Asm

object Reifier {

  case class Context(insns: Seq[String], vars: List[String])

  def reify(node: Ast.Node, vars: List[String] = Nil): Context = node match {

    case Ast.Declare(name, rhs) =>
      Context(reify(rhs, vars).insns, name :: vars)

    case Ast.Ident(name) => vars.indexOf(name) match {
      case -1 => throw new IllegalStateException(s"var $name undefined")
      case n => Context(List(s"LPush $n"), vars)
    }

    case Ast.Const(value: Int) =>
      Context(List(s"Push $value"), vars)

    case Ast.Const(value: String) =>
      // XXX: this is the same as above, strings come here quoted because
      //      of how the parser works, but that may change, so leaving
      //      separate with this note as a reminder
      Context(List(s"Push $value"), vars)

    case Ast.Add(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("Add")
      Context(insns, vars)

    case Ast.Sub(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("Sub")
      Context(insns, vars)

    case Ast.Mul(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("Mul")
      Context(insns, vars)

    case Ast.Div(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("Div")
      Context(insns, vars)

    case Ast.Gt(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("CmpGt")
      Context(insns, vars)

    case Ast.Gte(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("CmpGte")
      Context(insns, vars)

    case Ast.Eq(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("CmpEq")
      Context(insns, vars)

    case Ast.Neq(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("CmpNeq")
      Context(insns, vars)

    case Ast.Block(stmts, result) =>
      val initVars = vars
      // accumulate instructions and vars
      var ctxtSoFar = Context(Nil, initVars)
      stmts.foreach {
        case decl: Ast.Declare =>
          val declContext = reify(decl, ctxtSoFar.vars)
          ctxtSoFar = Context(ctxtSoFar.insns ++ declContext.insns, declContext.vars)
        case expr: Ast.Expr =>
          val exprContext = reify(expr, ctxtSoFar.vars)
          ctxtSoFar = Context((ctxtSoFar.insns ++ exprContext.insns) ++: List("Pop"), ctxtSoFar.vars)
      }

      // get instructions for final statement (has no effect on vars)
      val resultInsns = reify(result, ctxtSoFar.vars).insns

      // push value on top down while popping for return
      val resultPushInsns = List.range(0, ctxtSoFar.vars.size - initVars.size).map(_ => "Assign 0")

      // put em together proper (we've built the insns up reversed so far...
      val finalInsns = ctxtSoFar.insns ++ resultInsns ++ resultPushInsns
      Context(finalInsns, initVars)

    // UNTESTED, for now...
    case Ast.IfElse(cond, ifTrue, ifFalse) =>
      // XXX: this is gross, think this indicates a need to refactor this guy...
      val condInsns = reify(cond, vars).insns
      val ifTrueInsns = reify(ifTrue, vars).insns
      val ifFalseInsns = reify(ifFalse, vars).insns
      val labelPrefix = s"cond_${math.random}".replace(".", "_")
      val falseLabel = s"${labelPrefix}_F"
      val endLabel = s"${labelPrefix}_END"
      val insns = (condInsns ++ List(s"JmpF $falseLabel") ++
          ifTrueInsns ++ List(s"Jmp $endLabel") ++
          List(s"$falseLabel:") ++ ifFalseInsns ++ List(s"$endLabel:"))
      Context(insns, vars)

    case Ast.FunctionCall(name, argExprs) =>
      // function calls will expect args in reverse order
      val insns = pushAsArgs(argExprs.reverse, vars) ++ List(s"Jsr $name")
      Context(insns, vars)

    case Ast.TailCall(name, argExprs) =>
      val argsInsns = pushAsArgs(argExprs.reverse, vars)

      // assigns into the right slots
      val argOffsets = (vars.size - 1)
      val assignInsns = List.range(0, argExprs.size).map(_ => s"Assign $argOffsets")

      // pop off local vars
      val pops = List.range(0, vars.size - argExprs.size).map(_ => "Pop")

      Context(argsInsns ++ assignInsns ++ pops :+ s"Jmp $name", vars)

    case Ast.Function(name, args, body) =>
      val label = name + ":"
      // XXX: vars should always be empty here by present definition
      val bodyInsns = reify(body, args).insns
      val resultPushInsns = args.map(_ => "Assign 0")
      Context(label +: (bodyInsns ++ resultPushInsns :+ "Ret"), vars)

    case Ast.Program(functions) =>
      val functionInsns = functions.map(f => reify(f, Nil).insns)
      Context("Jsr main" :: "Push \"exit\"" ::  "CallBIF" :: functionInsns.flatten, Nil)

  }

  private def pushAsArgs(exprs: List[Ast.Expr], vars: List[String], asms: Seq[String] = Nil): Seq[String] = exprs match {
    case Nil => asms
    case arg :: rest =>
      // need to push pseudo-var "#nil" (not allowed per syntax) as a placeholder of stack location
      pushAsArgs(rest, "#nil" :: vars, asms ++ reify(arg, vars).insns)
  }

}
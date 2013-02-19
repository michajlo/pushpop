package org.michajlo.pushpop.lang

import org.michajlo.pushpop.vm.Asm

object Reifier {

  def reify(node: Ast.Node, vars: List[String] = Nil): (List[String], List[String]) = node match {

    case Ast.Declare(name, rhs) =>
      (reify(rhs, vars)._1, name :: vars)

    case Ast.Ident(name) => vars.indexOf(name) match {
      case -1 => throw new IllegalStateException("var " + name + " undefined")
      case n =>
        (List("LPush " + n), vars)
    }

    case Ast.Const(value: Int) =>
      (List("Push " + value), vars)

    case Ast.Const(value: String) =>
      // XXX: this is the same as above, strings come here quoted because
      //      of how the parser works, but that may change, so leaving
      //      separate with this note as a reminder
      (List("Push " + value), vars)

    case Ast.Add(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("Add")
      (insns, vars)

    case Ast.Sub(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("Sub")
      (insns, vars)

    case Ast.Mul(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("Mul")
      (insns, vars)

    case Ast.Div(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("Div")
      (insns, vars)

    case Ast.Gt(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("CmpGt")
      (insns, vars)

    case Ast.Gte(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("CmpGte")
      (insns, vars)

    case Ast.Eq(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("CmpEq")
      (insns, vars)

    case Ast.Neq(lhs, rhs) =>
      val insns = pushAsArgs(List(lhs, rhs), vars) ++ List("CmpNeq")
      (insns, vars)

    case Ast.Block(stmts, result) =>
      val initVars = vars
      // accumulate instructions and vars
      val (insns, endVars) = stmts.foldLeft((List[List[String]](), initVars)) {
        case ((insnsSoFar, varsSoFar), decl: Ast.Declare) =>
          val (declInsns, newVars) = reify(decl, varsSoFar)
          (declInsns :: insnsSoFar, newVars)
        case ((insnsSoFar, varsSoFar), expr: Ast.Expr) =>
          // this is being reversed
          (List("Pop") :: reify(expr, varsSoFar)._1 :: insnsSoFar, varsSoFar)
      }

      // get instructions for final statement (has no effect on vars)
      val resultInsns = reify(result, endVars)._1

      // push value on top down while popping for return
      val resultPushInsns = List.range(0, endVars.size - initVars.size).map(_ => "Assign 0")

      // put em together proper (we've built the insns up reversed so far...
      val finalInsns = (resultPushInsns :: resultInsns :: insns).reverse.flatten
      (finalInsns, initVars)

    // UNTESTED, for now...
    case Ast.IfElse(cond, ifTrue, ifFalse) =>
      // XXX: this is gross, think this indicates a need to refactor this guy...
      val condInsns = reify(cond, vars)._1
      val ifTrueInsns = reify(ifTrue, vars)._1
      val ifFalseInsns = reify(ifFalse, vars)._1
      val labelPrefix = ("cond_" + math.random).replace(".", "_")
      val falseLabel = labelPrefix + "_F"
      val endLabel = labelPrefix + "_END"
      val insns = (condInsns ++ List("JmpF " + falseLabel) ++
          ifTrueInsns ++ List("Jmp " + endLabel) ++
          List(falseLabel + ":") ++ ifFalseInsns ++ List(endLabel + ":"))
      (insns, vars)

    case Ast.FunctionCall(name, argExprs) =>
      // XXX: functions shouldn't have args from rest of call
      //      stack in scope, substitute with placeholders for reify?
      // function calls will expect args in reverse order
      val insns = pushAsArgs(argExprs.reverse, vars) ++ List("Jsr " + name)
      (insns, vars)

    case Ast.Function(name, args, body) =>
      val label = name + ":"
      // XXX: vars should always be empty here by present definition
      val bodyInsns = reify(body, args ++ vars)._1
      val resultPushInsns = args.map(_ => "Assign 0")
      (label :: (bodyInsns ++ resultPushInsns ++ List("Ret")), vars)

    case Ast.Program(functions) =>
      val functionInsns = functions.map(f => reify(f, Nil)._1)
      ("Jsr main" :: "Push \"exit\"" ::  "CallBIF" :: functionInsns.flatten, Nil)

  }

  private def pushAsArgs(exprs: List[Ast.Expr], vars: List[String], asms: List[List[String]] = Nil): List[String] = exprs match {
    case Nil => asms.reverse.flatten
    case arg :: rest =>
      // need to push pseudo-var "#nil" (not allowed per syntax) as a placeholder of stack location
      pushAsArgs(rest, "#nil" :: vars, reify(arg, vars)._1 :: asms)
  }

}
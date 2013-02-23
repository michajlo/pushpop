package org.michajlo.pushpop.lang

import Ast._

/**
 * Replaces all FunctionCalls in the tail position with a TailCall, which
 * is capable of reusing the current stack frame.
 */
object TailCallOptimizer {

  /**
   * Take a program and apply tail call optimization to all of its functions,
   * returning a new Program with optimizations applied
   */
  def optimize(prog: Program): Program = {
    val optimizedFunctions = prog.functions.map(optimizeFunction(_))
    prog.copy(optimizedFunctions)
  }

  private def optimizeFunction(function: Function): Function =
    function.copy(body = replaceTailCalls(function.name, function.body))


  private def replaceTailCalls(funName: String, block: Block): Block = block match {
    case Block(stmts, FunctionCall(name, args)) if name == funName =>
      Block(stmts, TailCall(name, args))

    case Block(stmts, IfElse(cnd, ifTrue, ifFalse)) =>
      Block(stmts,
        IfElse(cnd,
          replaceTailCalls(funName, ifTrue),
          replaceTailCalls(funName, ifFalse)
        )
      )

    case Block(stmts, subBlock: Block) =>
      Block(stmts, replaceTailCalls(funName, subBlock))

    case _ => block
  }
}
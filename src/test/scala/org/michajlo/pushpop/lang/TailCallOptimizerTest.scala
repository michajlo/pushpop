package org.michajlo.pushpop.lang
import org.scalatest.FunSpec

import Ast._

class TailCallOptimizerTest extends FunSpec {

  it ("must replace lone tail calls") {
    val function = Function("foo", List("a"),
      Block(
        List(Declare("x", Const(1))),
        FunctionCall("foo", List(Ident("x")))
      )
    )
    val program = Program(List(function))

    val expectedFunction = Function("foo", List("a"),
      Block(
        List(Declare("x", Const(1))),
        TailCall("foo", List(Ident("x")))
      )
    )
    val expectedProgram = Program(List(expectedFunction))

    assert(expectedProgram === TailCallOptimizer.optimize(program))
  }

  it ("must replace tail calls in hanging blocks") {
    val function = Function("foo", List("a"),
      Block(
        List(Declare("x", Const(1))),
        Block(
            List(Declare("y", Const(2))),
            FunctionCall("foo", List(Ident("x")))
        )
      )
    )
    val program = Program(List(function))

    val expectedFunction = Function("foo", List("a"),
       Block(
        List(Declare("x", Const(1))),
        Block(
            List(Declare("y", Const(2))),
            TailCall("foo", List(Ident("x")))
        )
      )
    )
    val expectedProgram = Program(List(expectedFunction))

    assert(expectedProgram === TailCallOptimizer.optimize(program))
  }

  it ("must replace tail calls in if else statements") {
    val function = Function("foo", List("a"),
      Block(
        List(Declare("x", Const(1))),
        IfElse(
            Gt(Ident("x"), Const(1)),
            Block(
              List(Declare("y", Const(2))),
              FunctionCall("foo", List(Ident("x")))
            ),
            Block(
              List(Declare("z", Const(3))),
              FunctionCall("foo", List(Ident("z")))
            )
        )
      )
    )
    val program = Program(List(function))

    val expectedFunction = Function("foo", List("a"),
       Block(
        List(Declare("x", Const(1))),
        IfElse(
            Gt(Ident("x"), Const(1)),
            Block(
              List(Declare("y", Const(2))),
              TailCall("foo", List(Ident("x")))
            ),
            Block(
              List(Declare("z", Const(3))),
              TailCall("foo", List(Ident("z")))
            )
        )
      )
    )
    val expectedProgram = Program(List(expectedFunction))

    assert(expectedProgram === TailCallOptimizer.optimize(program))
  }

    it ("must not replace non-tail calls") {
    val function = Function("foo", List("a"),
      Block(
        List(Declare("x", Const(1))),
        IfElse(
            Gt(Ident("x"), Const(1)),
            Block(
              List(FunctionCall("foo", List(Const(2)))),
              FunctionCall("bar", List(Ident("x")))
            ),
            Block(
              List(Declare("z", Const(3))),
              FunctionCall("foo", List(Ident("z")))
            )
        )
      )
    )
    val program = Program(List(function))

    val expectedFunction = Function("foo", List("a"),
       Block(
        List(Declare("x", Const(1))),
        IfElse(
            Gt(Ident("x"), Const(1)),
            Block(
              List(FunctionCall("foo", List(Const(2)))),
              FunctionCall("bar", List(Ident("x")))
            ),
            Block(
              List(Declare("z", Const(3))),
              TailCall("foo", List(Ident("z")))
            )
        )
      )
    )
    val expectedProgram = Program(List(expectedFunction))

    assert(expectedProgram === TailCallOptimizer.optimize(program))
  }
}
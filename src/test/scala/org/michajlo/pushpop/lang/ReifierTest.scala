package org.michajlo.pushpop.lang
import org.scalatest.FunSpec
import org.michajlo.pushpop.vm.Asm

class ReifierTest extends FunSpec {

  it ("must properly reify a declare with an int value") {
    val declare = Ast.Declare("varname", Ast.Const(1))

    val (insns, vars) = Reifier.reify(declare, Nil)

    assert(List("Push 1") === insns)
    assert(List("varname") === vars)
  }

  it ("must properly reify ident references") {
    val ident = Ast.Ident("varname")

    val vars = List("notvar", "varname")
    val (insns, newVars) = Reifier.reify(ident, vars)

    assert(List("LPush 1") === insns)
    assert(vars === newVars)
  }

  it ("must properly reify add nodes") {
    val vars = List("y", "x")
    val add = Ast.Add(Ast.Ident("x"), Ast.Const(1))

    val expected = List("LPush 1", "Push 1", "Add")

    val (insns, newVars) = Reifier.reify(add, vars)

    assert(expected === insns)
    assert(vars === newVars)
  }

  it ("must properly reify sub nodes") {
    val vars = List("y", "x")
    val sub = Ast.Sub(Ast.Ident("x"), Ast.Const(1))

    val expected = List("LPush 1", "Push 1", "Sub")

    val (insns, newVars) = Reifier.reify(sub, vars)

    assert(expected === insns)
    assert(vars === newVars)
  }

  it ("must properly reify mul nodes") {
    val vars = List("y", "x")
    val mul = Ast.Mul(Ast.Ident("x"), Ast.Ident("y"))

    val expected = List("LPush 1", "LPush 1", "Mul")

    val (insns, newVars) = Reifier.reify(mul, vars)

    assert(expected === insns)
    assert(vars === newVars)
  }

  it ("must properly reify div nodes") {
    val vars = List("y", "x")
    val div = Ast.Div(Ast.Ident("x"), Ast.Ident("y"))

    val expected = List("LPush 1", "LPush 1", "Div")

    val (insns, newVars) = Reifier.reify(div, vars)

    assert(expected === insns)
    assert(vars === newVars)
  }

  it ("must properly reify a block") {
    val vars = List("x", "y")
    val block = Ast.Block(
        List(
            Ast.Declare("x", Ast.Const(1)),
            Ast.Declare("y", Ast.Const(2)),
            Ast.Add(Ast.Ident("x"), Ast.Ident("y"))
        ),
        Ast.Ident("x")
    )

    val expected = List(
        "Push 1",    // decl x = 1
        "Push 2",    // decl x = 2
        "LPush 1",   // load x for add
        "LPush 1",   // load y for add
        "Add",
        "Pop",        // clear result of stmt
        "LPush 1",   // load x (Ident(x)
        "Assign 0",  // push final value down for return
        "Assign 0"   // ""
    )

    val (insns, newVars) = Reifier.reify(block, vars)

    assert(expected === insns)
    assert(vars == newVars)
  }

  it ("must properly reify a function call") {
    val vars = List("x", "y")
    val funcall = Ast.FunctionCall("foo", List(Ast.Const(1), Ast.Ident("x"), Ast.Ident("y")))

    val expected = List(
        "LPush 1",  // y
        "LPush 1",  // x
        "Push 1",   // 1
        "Jsr foo"
    )

    val (insns, newVars) = Reifier.reify(funcall, vars)

    assert(expected === insns)
    assert(vars == newVars)
  }

  it ("must properly reify a function") {
    val vars = List("x", "y")
    val fun = Ast.Function("foo", List("arg1", "arg2"),
        Ast.Block(
            List(
                Ast.Declare("z", Ast.Ident("arg2")),
                Ast.Declare("a", Ast.Ident("arg1")),
                Ast.Declare("b", Ast.Ident("arg2"))
            ),
            Ast.Ident("z")
        )
    )

    val expected = List(
        "foo:",
        "LPush 1",  // z = arg2
        "LPush 1",  // a = arg1
        "LPush 3",  // b = arg2
        "LPush 2",  // z
        "Assign 0", // replace b stack slot
        "Assign 0", // replace a "        "
        "Assign 0", // replace z "        "
        "Assign 0", // replace arg2 stack slot
        "Assign 0", // replace arg1 stack slot
        "Ret"
    )

    val (insns, newVars) = Reifier.reify(fun, vars)

    assert(expected === insns)
    assert(vars == newVars)
  }

  it ("must properly reify a program") {
    val function1 = Ast.Function("foo", List("arg"), Ast.Block(Nil, Ast.Const(1)))
    val function2 = Ast.Function("main", List(), Ast.Block(Nil, Ast.Const(2)))

    val prog = Ast.Program(List(function1, function2))

    val expected = List(
        "Jsr main",
        "Push \"exit\"",
        "CallBIF",
        "foo:",
        "Push 1",
        "Assign 0",
        "Ret",
        "main:",
        "Push 2",
        "Ret"
    )

    val (insns, newVars) = Reifier.reify(prog, Nil)

    assert(expected === insns)
    assert(Nil === newVars)
  }
}
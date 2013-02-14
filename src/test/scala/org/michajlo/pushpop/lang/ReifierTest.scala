package org.michajlo.pushpop.lang
import org.scalatest.FunSpec
import org.michajlo.pushpop.vm.Asm

class ReifierTest extends FunSpec {

  it ("must properly reify a declare with a value") {
    val declare = Ast.Declare("varname", Ast.Const(1))

    val (insns, vars) = Reifier.reify(declare, Nil)

    assert(List(Asm.Push(1)) === insns)
    assert(List("varname") === vars)
  }

  it ("must properly reify ident references") {
    val ident = Ast.Ident("varname")

    val vars = List("notvar", "varname")
    val (insns, newVars) = Reifier.reify(ident, vars)

    assert(List(Asm.LPush(1)) === insns)
    assert(vars === newVars)
  }

  it ("must properly reify add nodes") {
    val vars = List("y", "x")
    val add = Ast.Add(Ast.Ident("x"), Ast.Const(1))

    val expected = List(Asm.LPush(1), Asm.Push(1), Asm.Add)

    val (insns, newVars) = Reifier.reify(add, vars)

    assert(expected === insns)
    assert(vars === newVars)
  }

  it ("must properly reify sub nodes") {
    val vars = List("y", "x")
    val sub = Ast.Sub(Ast.Ident("x"), Ast.Const(1))

    val expected = List(Asm.LPush(1), Asm.Push(1), Asm.Sub)

    val (insns, newVars) = Reifier.reify(sub, vars)

    assert(expected === insns)
    assert(vars === newVars)
  }

  it ("must properly reify mul nodes") {
    val vars = List("y", "x")
    val mul = Ast.Mul(Ast.Ident("x"), Ast.Ident("y"))

    val expected = List(Asm.LPush(1), Asm.LPush(1), Asm.Mul)

    val (insns, newVars) = Reifier.reify(mul, vars)

    assert(expected === insns)
    assert(vars === newVars)
  }

  it ("must properly reify div nodes") {
    val vars = List("y", "x")
    val div = Ast.Div(Ast.Ident("x"), Ast.Ident("y"))

    val expected = List(Asm.LPush(1), Asm.LPush(1), Asm.Div)

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
        Asm.Push(1),    // decl x = 1
        Asm.Push(2),    // decl x = 2
        Asm.LPush(1),   // load x for add
        Asm.LPush(1),   // load y for add
        Asm.Add,
        Asm.Pop,        // clear result of stmt
        Asm.LPush(1),   // load x (Ident(x)
        Asm.Assign(0),  // push final value down for return
        Asm.Assign(0)   // ""
    )

    val (insns, newVars) = Reifier.reify(block, vars)

    assert(expected === insns)
    assert(vars == newVars)
  }
}
package org.michajlo.pushpop.lang
import org.scalatest.FunSpec
import org.michajlo.pushpop.vm.Asm

class ReifierTest extends FunSpec {

  it ("must properly reify a declare with a value") {
    val declare = Ast.Declare("varname", Some(Ast.Const(1)))

    val (insns, vars) = Reifier.reify(declare, Nil)

    assert(List(Asm.Push(1)) === insns)
    assert(List("varname") === vars)
  }

  it ("must properly reify a declare without a value") {
    val declare = Ast.Declare("varname", None)

    val (insns, vars) = Reifier.reify(declare, Nil)

    assert(List(Asm.Push(null)) === insns)
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
}
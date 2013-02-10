package org.michajlo.pushpop.lang
import org.scalatest.FunSpec
import org.michajlo.pushpop.vm.Asm

class ReifierTest extends FunSpec {

  it ("must properly reify a declare with a value") {
    val declare = Ast.Declare("varname", Some(1))

    val (insns, vars) = Reifier.reifyDeclare(declare, Nil)

    assert(List(Asm.Push(1)) === insns)
    assert(List("varname") === vars)
  }

  it ("must properly reify a declare without a value") {
    val declare = Ast.Declare("varname", None)

    val (insns, vars) = Reifier.reifyDeclare(declare, Nil)

    assert(List(Asm.Push(null)) === insns)
    assert(List("varname") === vars)
  }

  it ("must properly reify ident references") {
    val ident = Ast.Ident("varname")

    val vars = List("notvar", "varname")
    val (insns, newVars) = Reifier.reifyIdent(ident, vars)

    assert(List(Asm.LPush(1)) === insns)
    assert(vars === newVars)
  }
}
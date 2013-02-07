package org.michajlo.pushpop.asm
import org.scalatest.FunSuite
import org.scalatest.FunSpec
import org.michajlo.pushpop.vm.Asm

class PlaintextAsmParserTest extends FunSpec {

  it ("must fail on missing data section") {
    val assembly = """
      code {
        Push 1
      }
      """

    intercept [IllegalArgumentException] {
      PlaintextAsmParser.parse(assembly)
    }
  }

  it ("must not fail on empty data section") {
    val assembly = """
      data {}
      code {
        Push 1
      }
      """

    PlaintextAsmParser.parse(assembly)
  }

  it ("must properly assembly instructions, filling in transient instructions from data") {
    val assembly = """
      data {
        aString1 = "stringy"
        aNumber2 = 43
      }
      code {
        Push 1
        LPush aString1
        LPush aNumber2
        Pop
        Add
        Sub
        Mul
        Div
        CallBIF
      }
      """

    val expected = List(
        Asm.Push(1),
        Asm.Push("stringy"),
        Asm.Push(43),
        Asm.Pop,
        Asm.Add,
        Asm.Sub,
        Asm.Mul,
        Asm.Div,
        Asm.CallBIF)


    val insns = PlaintextAsmParser.parse(assembly)

    assert(expected === insns)
  }

  it ("must properly fill in labels") {
    val assembly = """
      data {}
      code {
        label1:
        label2:
          Push 1
          Jsr label1
        label3:
          Jsr label2
          Jsr label3
      }
      """

    val expected = List(
        Asm.Push(1),
        Asm.Jsr(0),
        Asm.Jsr(0),
        Asm.Jsr(2))

    val insns = PlaintextAsmParser.parse(assembly)

    assert(expected === insns)
  }
}
package org.michajlo.pushpop.asm
import org.scalatest.FunSuite
import org.scalatest.FunSpec
import org.michajlo.pushpop.vm.Asm

class PlaintextAsmParserTest extends FunSpec {


  it ("must properly assemble instructions") {
    val assembly = """
      Push "stringy"
      Push 42
      Pop
      Add
      Sub
      Mul
      Div
      CallBIF
      Jmp 1
      JmpZ 2
      Assign 3
      LPush 4
      Jsr 5
      Ret
      """

    val expected = List(
        Asm.Push("stringy"),
        Asm.Push(42),
        Asm.Pop,
        Asm.Add,
        Asm.Sub,
        Asm.Mul,
        Asm.Div,
        Asm.CallBIF,
        Asm.Jmp(1),
        Asm.JmpZ(2),
        Asm.Assign(3),
        Asm.LPush(4),
        Asm.Jsr(5),
        Asm.Ret)


    val insns = PlaintextAsmParser.parse(assembly)

    assert(expected === insns)
  }

  it ("must properly fill in labels") {
    val assembly = """
      label1:
        Jsr label1
      label2:
        Jmp label2
      label3:
        JmpZ label3
      """

    val expected = List(
        Asm.Jsr(0),
        Asm.Jmp(1),
        Asm.JmpZ(2))

    val insns = PlaintextAsmParser.parse(assembly)

    assert(expected === insns)
  }

  it ("must ignore comments") {
    val assembly = """
      label1:; this is the first label
      label2: ;this is another label
      ; this is a comment in the middle of nowhere

      ; this is a comment after an empty line
      Push 1
      Push 2 ; this is a comment after an insn
      Push "hello"; this is a comment after another insn
      ;Push 5 this is a commented out instruction
      Jsr label1 ; and a jump
      Jsr ; this is legal
      label2 ; but terribly immoral
      """

    val expected = List(
        Asm.Push(1),
        Asm.Push(2),
        Asm.Push("hello"),
        Asm.Jsr(0),
        Asm.Jsr(0)
    )

    val insns = PlaintextAsmParser.parse(assembly)

    assert(expected === insns)
  }
}
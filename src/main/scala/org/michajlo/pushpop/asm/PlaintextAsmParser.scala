package org.michajlo.pushpop.asm
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import org.michajlo.pushpop.vm.Asm
import java.io.StringReader
import java.io.Reader

object PlaintextAsmParser extends JavaTokenParsers {
  def parse(assembly: String): List[Asm.Insn] = parse(new StringReader(assembly))

  def parse(assembly: Reader): List[Asm.Insn] =
    (new PlaintextAsmParser).parse(assembly)
}

/**
 * Parser for plaintext pushpop assembly code.
 */
class PlaintextAsmParser extends JavaTokenParsers {

  /**
   * Load instructions from a string, throw IllegalArgumentException on parse error,
   * or NoSuchElementException on bad data reference in the code section
   *
   * @param assembly String assembly code representation
   *
   * @return list of Asm.Insns from input
   */
  def parse(assembly: String): List[Asm.Insn] = parse(new StringReader(assembly))

  /**
   * Load instructions from a Reader, throw IllegalArgumentException on parse error,
   * or NoSuchElementException on bad data reference in the code section
   *
   * @param assembly Reader from which assembly can be read
   *
   * @return list of Asm.Insns from input
   */
  def parse(assembly: Reader): List[Asm.Insn] = parseAll(code, assembly) match {
    case Success(insns, _) => reify(insns, getLabelOffsets(insns))
    case nonSuccess => throw new IllegalArgumentException("Error parsing assembly: " + nonSuccess)
  }

  // Parent trait for compilation
  trait Intermediary
  case class Label(name: String) extends Intermediary
  case class FullInsn(insn: Asm.Insn) extends Intermediary
  case class PartialInsn(name: String, arg: Option[Any]) extends Intermediary

  override val whiteSpace = """(\s*(;[^\n]*\n)?)+""".r

  // a label identifies a region of code
  private def label: Parser[Label] = ident <~ ":" ^^ { Label(_) }

  // a properInsn is something that can be translated directly to assembly
  //  with no additional passes
  private def properInsn: Parser[Asm.Insn] =
    ("Push" ~> wholeNumber ^^ { n => Asm.Push(n.toInt) }) |
    ("Push" ~> stringLiteral ^^ { s => Asm.Push(s.subSequence(1, s.length() - 1))}) |
    ("LPush" ~> wholeNumber ^^ { n => Asm.LPush(n.toInt) }) |
    ("Assign" ~> wholeNumber ^^ { n => Asm.Assign(n.toInt) }) |
    ("Pop" ^^ { _ => Asm.Pop }) |
    ("Add" ^^ { _ => Asm.Add }) |
    ("Sub" ^^ { _ => Asm.Sub }) |
    ("Mul" ^^ { _ => Asm.Mul }) |
    ("Div" ^^ { _ => Asm.Div }) |
    ("CallBIF" ^^ { _ => Asm.CallBIF } ) |
    ("Jsr" ~> wholeNumber ^^ { n => Asm.Jsr(n.toInt) }) |
    ("Ret" ^^ { _ => Asm.Ret }) |
    ("JmpZ" ~> wholeNumber ^^ { n => Asm.JmpZ(n.toInt) }) |
    ("Jmp" ~> wholeNumber ^^ { n => Asm.Jmp(n.toInt) })

  // a fullInsn is simply a wrapper to Asm.Insn so it falls under the Intermediary type
  private def fullInsn: Parser[FullInsn] = properInsn ^^ { FullInsn(_) }

  // an instruction that needs a second pass (label substitution) to become a proper instruction
  private def partialInsn: Parser[PartialInsn] =
    ("Jsr" ~> ident ^^ { lbl => PartialInsn("Jsr", Some(lbl)) }) |
    ("JmpZ" ~> ident ^^ { lbl => PartialInsn("JmpZ", Some(lbl) )}) |
    ("Jmp" ~> ident ^^ { lbl => PartialInsn("Jmp", Some(lbl)) })


  // all of the codes, all together
  private def code: Parser[List[Intermediary]] = rep1(fullInsn | partialInsn | label)

  // post-processing
  private def getLabelOffsets(intermediaries: List[Intermediary]) = {
    val (_, labelOffsets) = intermediaries.foldLeft((0, Map[String, Int]())) {
      case ((off, offsets), Label(label)) =>
        (off, offsets + (label -> off))
      case ((off, tokensOffsets), _: PartialInsn | _: FullInsn) =>
        (off + 1, tokensOffsets)
    }
    labelOffsets
  }

  def reify(intermediaries: List[Intermediary], labelOffsets: Map[String, Int]) = intermediaries.collect {
    case FullInsn(insn) => insn
    case PartialInsn("Jsr", Some(lbl: String)) => Asm.Jsr(labelOffsets(lbl))
    case PartialInsn("Jmp", Some(lbl: String)) => Asm.Jmp(labelOffsets(lbl))
    case PartialInsn("JmpZ", Some(lbl: String)) => Asm.JmpZ(labelOffsets(lbl))
  }

}
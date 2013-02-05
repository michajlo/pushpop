package org.michajlo.pushpop.asm
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import org.michajlo.pushpop.vm.Asm
import java.io.StringReader
import java.io.Reader

/**
 * Parser for plaintext pushpop assembly code. Expected format is
 *
 *   data {
 *     (ident = (stringLiteral | wholeNumber))*
 *   }
 *
 *   code {
 *     insn+
 *   }
 *
 * Where insn is one of the native instructions
 *
 *   Push wholeNumber
 *   Pop
 *   Add
 *   Sub
 *   Mul
 *   Div
 *   CallBIF
 *
 * or the pseudo instruction
 *
 *   LPush ident
 *
 * Where LPush ident translates into Push(data[key])
 *
 * Note that while the data section may be empty, it may not be omitted.
 */
object PlaintextAsmParser extends JavaTokenParsers {

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
  def parse(assembly: Reader): List[Asm.Insn] = parse(dataSection, assembly) match {
    case Success(data, rest) => parseAll(codeSection(data), rest) match {
      case Success(insns, _) => insns
      case nonSuccess => throw new IllegalArgumentException("Error parsing code section: " + nonSuccess)
    }
    case nonSuccess => throw new IllegalArgumentException("Error parsing data section: " + nonSuccess)
  }


  // note that for string literal we get the preceding and trailing "
  private def dataValue: Parser[Any] =
    (stringLiteral ^^ {s => s.subSequence(1, s.length() - 1)}) |
    (wholeNumber ^^ {s => s.toInt})

  private def dataAssignment: Parser[(String, Any)] = ident ~ "=" ~ dataValue ^^ {
    case (key ~ "=" ~ value) => (key, value)
  }

  private def dataSection: Parser[Map[String, Any]] =
    ("data" ~ "{") ~> rep(dataAssignment) <~ "}" ^^ {
      kvs => Map[String, Any](kvs: _*)
    }

  private def insn(data: Map[String, Any]): Parser[Asm.Insn] =
    ("Push" ~> wholeNumber ^^ { n => Asm.Push(n.toInt) }) |
    ("LPush" ~> ident ^^ { id => Asm.Push(data(id))}) |
    ("Pop" ^^ { _ => Asm.Pop }) |
    ("Add" ^^ { _ => Asm.Add }) |
    ("Sub" ^^ { _ => Asm.Sub }) |
    ("Mul" ^^ { _ => Asm.Mul }) |
    ("Div" ^^ { _ => Asm.Div }) |
    ("CallBIF" ^^ { _ => Asm.CallBIF} )

  private def codeSection(data: Map[String, Any]): Parser[List[Asm.Insn]] =
    ("code" ~ "{") ~> rep1(insn(data)) <~ "}"

}
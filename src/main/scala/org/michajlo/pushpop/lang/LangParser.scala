package org.michajlo.pushpop.lang
import scala.util.parsing.combinator.JavaTokenParsers
import Ast._
import java.io.StringReader
import org.michajlo.pushpop.vm.Asm
import java.io.Reader

object LangParser {

  def parse(source: String): Program = parse(new StringReader(source))

  def parse(source: Reader): Program =
    (new LangParser).parse(source)
}

class LangParser extends JavaTokenParsers {

   /**
   * Load program from a string
   *
   * @param source code
   *
   * @return the Program ast represented by source code
   */
  def parse(assembly: String): Program = parse(new StringReader(assembly))

  /**
   * Load source from a reader. Throws IllegalArgument on parse error,
   * IllegalStateException on bad code structure
   *
   * @param source Reader from which source can be read
   *
   * @return Program AST representation
   */
  def parse(assembly: Reader): Program = parseAll(program, assembly) match {
    case Success(ast, _) => ast
    case nonSuccess => throw new IllegalArgumentException("Error parsing source: " + nonSuccess)
  }


  def constInt: Parser[Const] = wholeNumber ^^ { i => Const(i.toInt) }

  // TODO: promote ConsString to own type?
  def constString: Parser[Const] = stringLiteral ^^ { s => Const(s) }

  def identRef: Parser[Ident] = ident ^^ { id => Ident(id) }

  def funCall: Parser[FunctionCall] = ident ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case name ~ "(" ~ argExprs ~ ")" => FunctionCall(name, argExprs)
  }

  def arithExpr: Parser[Expr] = chainl1(multOrDivExpr, ("+" | "-") ^^ {
    case "+" => (lhs, rhs) => Add(lhs, rhs)
    case "-" => (lhs, rhs) => Sub(lhs, rhs)
  })

  def multOrDivExpr: Parser[Expr] = chainl1(arithAtom, ("*" | "/") ^^ {
    case "*" => (lhs, rhs) => Mul(lhs, rhs)
    case "/" => (lhs, rhs) => Div(lhs, rhs)
  })

  def arithAtom: Parser[Expr] = (block | funCall | constInt | identRef | "(" ~> arithExpr <~ ")")

  def cmpExpr: Parser[Expr] =
    (block | arithExpr | funCall | constInt | identRef) ~
      ("<=" | "<" | ">=" | ">" | "==" | "!=") ~
      (block | arithExpr | funCall | constInt | identRef) ^^ {

    case lhs ~ "<=" ~ rhs => Gt(rhs, lhs)
    case lhs ~ "<" ~ rhs => Gte(rhs, lhs)
    case lhs ~ ">=" ~ rhs => Gte(lhs, rhs)
    case lhs ~ ">" ~ rhs => Gt(lhs, rhs)
    case lhs ~ "==" ~ rhs => Eq(lhs, rhs)
    case lhs ~ "!=" ~ rhs => Neq(lhs, rhs)
  }

  def ifElse: Parser[IfElse] = "if" ~ "(" ~ cmpExpr ~ ")" ~ block ~ "else" ~ block ^^ {
    case _ ~ _ ~ cmpExpr ~ _ ~ ifTrue ~ _ ~ ifFalse => IfElse(cmpExpr, ifTrue, ifFalse)
  }

  def expr: Parser[Expr] = (block | ifElse | arithExpr | funCall | constInt | constString | identRef)

  def declare: Parser[Declare] = "let" ~> (ident ~ ":=" ~ expr) ^^ {
    case name ~ ":=" ~ v => Declare(name, v)
  }

  def stmt: Parser[Stmt] = (declare | expr)

  def block: Parser[Block] = "{" ~> repsep(stmt, ";") <~ (opt(";") ~ "}") ^^ {
    // this is gross, i'll come up with something better later...
    case statements => statements.reverse match {
      case (retval: Expr) :: revStatements => Block(revStatements.reverse, retval)
      case _ => throw new IllegalStateException("Expression required at end of block")
    }
  }

  def function: Parser[Function] = ident ~ "(" ~ repsep(ident, ",") ~ ")" ~ block ^^ {
    case name ~ "(" ~ argNames ~ ")" ~ body => Function(name, argNames, body)
  }

  def program: Parser[Program] = rep1(function) ^^ { Program(_) }

}
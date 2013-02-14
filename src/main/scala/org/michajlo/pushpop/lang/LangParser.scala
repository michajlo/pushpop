package org.michajlo.pushpop.lang
import scala.util.parsing.combinator.JavaTokenParsers

import Ast._

object LangParser extends JavaTokenParsers {

  def constInt: Parser[Const] = wholeNumber ^^ { i => Const(i.toInt) }

  // TODO: promote ConsString to own type?
  def constString: Parser[Const] = stringLiteral ^^ { s => Const(s) }

  def identRef: Parser[Ident] = ident ^^ { id => Ident(id) }

  def funCall: Parser[FunctionCall] = ident ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case name ~ "(" ~ argExprs ~ ")" => FunctionCall(name, argExprs)
  }

  def expr: Parser[Expr] = (constInt | constString | funCall | identRef)

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
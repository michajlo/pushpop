package org.michajlo.pushpop.lang

object Ast {

  /**
   * High level trait encompassing all the things,
   * probably not the best name, but will do
   */
  trait Node

  /**
   * Anything that can be run
   */
  trait Stmt extends Node

  /**
   * Declare a variable, has side effect of creating a variable for this
   * context
   */
  case class Declare(ident: String, value: Expr) extends Stmt

  /**
   * Something that has a value (leaves a value on the stack)
   */
  trait Expr extends Stmt

  /**
   * Variable reference
   */
  case class Ident(ident: String) extends Expr

  /**
   * Constant value
   */
  case class Const(v: Any) extends Expr

  /**
   * Arithmetic expressions
   */
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr
  case class Mul(lhs: Expr, rhs: Expr) extends Expr
  case class Div(lhs: Expr, rhs: Expr) extends Expr

  /**
   * A chunk of sequentially executed code, returning a value
   */
  case class Block(stmts: List[Stmt], value: Expr) extends Expr

  /**
   * Function declaration
   */
  case class Function(name: String, args: List[String], body: Block) extends Node

  /**
   * Function call
   */
  case class FunctionCall(name: String, args: List[Expr]) extends Expr

  /**
   * High level description of a program
   */
  case class Program(functions: List[Function]) extends Node
}
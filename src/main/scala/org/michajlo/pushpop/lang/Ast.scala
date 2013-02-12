package org.michajlo.pushpop.lang

object Ast {

  trait Node

  case class Declare(ident: String, value: Option[Node]) extends Node

  case class Ident(ident: String) extends Node

  case class Const(v: Any) extends Node

  case class Add(lhs: Node, rhs: Node) extends Node

  case class Sub(lhs: Node, rhs: Node) extends Node

  case class Mul(lhs: Node, rhs: Node) extends Node

  case class Div(lhs: Node, rhs: Node) extends Node
}
package org.michajlo.pushpop.lang

object Ast {

  trait Node

  case class Declare(ident: String, value: Option[Any]) extends Node

  case class Ident(ident: String) extends Node

  case class Const(v: Any) extends Node
}
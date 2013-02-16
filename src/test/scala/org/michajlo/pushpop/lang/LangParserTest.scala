package org.michajlo.pushpop.lang
import org.scalatest.FunSpec

import Ast._

class LangParserTest extends FunSpec {

  it ("must properly parse a block of random instructions") {
    val code = """
      {
        let x := 1;
        let y := 2;
        let z := x + y;
        let a := z * (x + 1) / 2;
        function(funfun(1), z + a, "a", x);
        a
      }
      """

      val expected = Block(
          List(
              Declare("x", Const(1)),
              Declare("y", Const(2)),
              Declare("z", Add(Ident("x"), Ident("y"))),
              Declare("a", Div(Mul(Ident("z"), Add(Ident("x"), Const(1))), Const(2))),
              FunctionCall("function", List(
                  FunctionCall("funfun", List(Const(1))),
                  Add(Ident("z"), Ident("a")),
                  Const("\"a\""),
                  Ident("x")
              ))
          ),
          Ident("a")
      )

      val parser = new LangParser
      val parsed = parser.parseAll(parser.block, code)

      assert(expected === parsed.get)
  }
}
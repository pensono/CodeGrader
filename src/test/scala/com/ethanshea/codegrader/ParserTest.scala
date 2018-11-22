package com.ethanshea.codegrader

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

object ParserTest {
  @Test
  def parseLiterals(): Unit = {
    assertEquals(Identifier("car"), p("car"))
    assertEquals(Identifier("fast-car"), p("fast-car"))

    assertEquals(Literal("5"), p("5"))
    assertEquals(Literal("#f"), p("#f"))
    assertEquals(Literal("\"string\""), p("\"string\""))

    assertEquals(MetaVariable("expr"), p("$expr"))
  }

  @Test
  def parseSexp() : Unit = {
    assertEquals(2, p("(car xs)").asInstanceOf[SExpression].children.size)
    assertEquals(3, p("(car 5 xs)").asInstanceOf[SExpression].children.size)
    assertEquals(3, p("(car 5 (cdr xs))").asInstanceOf[SExpression].children.size)
  }
}

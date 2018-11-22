package com.ethanshea.codegrader

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

import com.ethanshea.codegrader.RacketMetaSyntax.parseExpression

object ParserTest {
  @Test
  def parseLiterals(): Unit = {
    assertEquals(Literal("5"), parseExpression("5"))
    assertEquals(Identifier("car"), parseExpression("car"))
    assertEquals(Identifier("fast-car"), parseExpression("fast-car"))
    assertEquals(Literal("#f"), parseExpression("#f"))
    assertEquals(Literal("\"string\""), parseExpression("\"string\""))
  }

  @Test
  def parseSexp() : Unit = {
    assertEquals(2, parseExpression("(car xs)").asInstanceOf[SExpression].children.size)
    assertEquals(3, parseExpression("(car 5 xs)").asInstanceOf[SExpression].children.size)
    assertEquals(3, parseExpression("(car 5 (cdr xs))").asInstanceOf[SExpression].children.size)
  }
}

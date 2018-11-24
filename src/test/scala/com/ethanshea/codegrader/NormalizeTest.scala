package com.ethanshea.codegrader

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

object NormalizeTest {

  @Test
  def basicMatch(): Unit = {
    assertTrue(equiv(List(basicSimplify), p("(+ 1 0)"), p("1")))
    assertFalse(equiv(List(basicSimplify), p("(+ 1 1)"), p("2")))

    assertTrue(equiv(List(basicSimplify), p("(+ (+ 1 0) 0)"), p("1")))
    assertTrue(equiv(List(basicSimplify), p("(* 2 (+ 1 0))"), p("(* 2 1)")))


    assertTrue(equiv(List(basicSimplify), p("(* 2 (+ (+ 1 0) 0))"), p("(* 2 1)")))
  }

  @Test
  def variableMatch(): Unit = {
    assertTrue(equiv(List(additiveIdentity), p("(* 2 (+ 1 0))"), p("(* 2 1)")))
    assertTrue(equiv(List(additiveIdentity), p("(* 2 (+ 42 0))"), p("(* 2 42)")))
  }
}

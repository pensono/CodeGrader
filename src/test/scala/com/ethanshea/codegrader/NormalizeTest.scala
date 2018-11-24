package com.ethanshea.codegrader

import com.ethanshea.codegrader.EquivalenceSubstitution.matchExpr
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

object NormalizeTest {
  val negateEquiv = new RacketEquivalence("(- $x)", "(- 0 $x)", "(* -1 $x)")
  val basicSimplify = new RacketEquivalence("(+ 1 0)", "1") // Not generalizable, but still useful for these tests

  @Test
  def basicMatch(): Unit = {

    assertTrue(equiv(List(basicSimplify), p("(* 2 (+ 1 0))"), p("(* 2 1)")))

    assertTrue(equiv(List(basicSimplify), p("(+ 1 0)"), p("1")))
    assertFalse(equiv(List(basicSimplify), p("(+ 1 1)"), p("2")))

    assertTrue(equiv(List(basicSimplify), p("(+ (+ 1 0) 0)"), p("1")))

    assertTrue(equiv(List(basicSimplify), p("(* 2 (+ (+ 1 0) 0))"), p("(* 2 1)")))
  }
}

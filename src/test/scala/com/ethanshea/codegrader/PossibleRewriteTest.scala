package com.ethanshea.codegrader

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

object PossibleRewriteTest {
  @Test
  def strangeRewriteShouldWork(): Unit = {
    // Seems weird, but the innermost + is being replated with (+ 0 +).
    assertTrue(possibleRewrite(additiveIdentity, p("(* 2 (+ 0 (+ 1 0)))"), p("(* 2 (+ 0 ((+ 0 +) 1 0)))")))
    assertTrue(possibleRewrite(additiveIdentity, p("+"), p("(+ 0 +)")))
    assertTrue(possibleRewrite(additiveIdentity, p("+"), p("(+ + 0)")))
  }

  @Test
  def basicRewrites(): Unit = {
    assertTrue(possibleRewrite(additiveIdentity, p("(* 2 (+ 0 1))"), p("(* 2 1)")))
    assertTrue(possibleRewrite(additiveIdentity, p("(* 2 1)"), p("(* 2 (+ 0 1))")))
  }

  @Test
  def dontRewriteToSelf(): Unit = {
    assertFalse(possibleRewrite(additiveIdentity, p("4"), p("4")))
  }
}

package com.ethanshea.codegrader

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import com.ethanshea.codegrader.EquivalenceSubstitution.matchExpr

object MatchTest {
  @Test
  def basicMatch(): Unit = {
    assertEquals(Some(Map.empty), matchExpr(p("1"), p("1")))
    assertEquals(None, matchExpr(p("1"), p("2")))

    assertEquals(Some(Map.empty), matchExpr(p("car"), p("car")))
    assertEquals(None, matchExpr(p("car"), p("cdr")))

    assertEquals(Some(Map.empty), matchExpr(p("\"string\""), p("\"string\"")))
    assertEquals(None, matchExpr(p("\"string\""), p("\"other string\"")))

    assertEquals(None, matchExpr(p("\"string\""), p("1")))
    assertEquals(None, matchExpr(p("car"), p("1")))
  }

  @Test
  def sexpMatch() : Unit = {
    assertEquals(Some(Map.empty), matchExpr(p("(car ls)"), p("(car ls)")))
    assertEquals(Some(Map.empty), matchExpr(p("(car ls \"lol\")"), p("(car ls \"lol\")")))
    assertEquals(None, matchExpr(p("(car \"lol\" ls)"), p("(car ls \"lol\")")))
    assertEquals(None, matchExpr(p("(car ls)"), p("(car ls \"lol\")")))
    assertEquals(None, matchExpr(p("(car \"lol\" ls)"), p("(car ls)")))

    assertEquals(None, matchExpr(p("(car \"lol\" ls)"), p("car")))
    assertEquals(None, matchExpr(p("(car \"lol\" ls)"), p("4")))
  }

  @Test
  def variableBinding() : Unit = {
    assertEquals(Some(Map("x" -> p("xs"))), matchExpr(p("(car xs)"), p("(car $x)")))
    assertEquals(None, matchExpr(p("(zip xs ys)"), p("(car $x $y)")))
    assertEquals(Some(Map("x" -> p("xs"), "y" -> p("ys"))), matchExpr(p("(zip xs ys)"), p("(zip $x $y)")))
    assertEquals(Some(Map("x" -> p("(xs)"))), matchExpr(p("(car (xs))"), p("(car $x)")))
    assertEquals(Some(Map("x" -> p("xs"))), matchExpr(p("(car (xs))"), p("(car ($x))")))
  }

  @Test
  def multipleVariableOccurrences() : Unit = {
    assertEquals(Some(Map("x" -> p("xs"))), matchExpr(p("(car xs xs)"), p("(car $x $x)")))

    assertEquals(None, matchExpr(p("(car xs ys)"), p("(car $x $x)")))
    assertEquals(None, matchExpr(p("(car xs xs)"), p("(car $x ($x))")))
    assertEquals(Some(Map("x" -> p("xs"))), matchExpr(p("(car xs (xs))"), p("(car $x ($x))")))

    assertEquals(None, matchExpr(p("(car xs (ys))"), p("(car $x $x)")))
    assertEquals(None, matchExpr(p("(car xs (xs))"), p("(car $x $x)")))
    assertEquals(Some(Map("x" -> p("xs"))), matchExpr(p("(car xs (xs))"), p("(car $x ($x))")))
  }
}

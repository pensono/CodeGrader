package com.ethanshea.codegrader

class RacketEquivalence(val members: Set[RacketMetaExpression]) {
  def this(expressions: String*) {
    this(expressions.map(RacketMetaSyntax.parseExpression).toSet)
  }
}

object RacketEquivalence {
  val equivalences = List(
    new RacketEquivalence("(- $x)", "(- 0 $x)", "(* -1 $x)")
  )
}

package com.ethanshea

package object codegrader {
  def p(input: String) : RacketMetaExpression = RacketMetaSyntax.parseExpression(input)
  def equiv(equivalences: List[RacketEquivalence], a: RacketMetaExpression, b: RacketMetaExpression) : Boolean =
    EquivalenceSubstitution.normalizeExpr(a, b, equivalences).isDefined
}

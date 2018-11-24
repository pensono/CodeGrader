package com.ethanshea

package object codegrader {
  def p(input: String) : RacketMetaExpression = RacketMetaSyntax.parseExpression(input)
  def equiv(racketEquivalence: RacketEquivalence, a: RacketMetaExpression, b: RacketMetaExpression) =
    EquivalenceSubstitution.equivalent(a, b, racketEquivalence)
}

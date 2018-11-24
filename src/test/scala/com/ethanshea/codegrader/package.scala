package com.ethanshea

import com.ethanshea.codegrader.EquivalenceSubstitution.RacketRewrite

package object codegrader {
  // Equivalences
  val negateEquiv = new RacketEquivalence("(- $x)", "(- 0 $x)", "(* -1 $x)")
  val basicSimplify = new RacketEquivalence("(+ 1 0)", "1") // Not generalizable, but still useful for these tests
  val additiveIdentity = new RacketEquivalence("(+ $x 0)", "(+ 0 $x)", "$x")
  val multiplicativeIdentity = new RacketEquivalence("(* $x 1)", "(* 1 $x)", "$x")

  def p(input: String) : RacketMetaExpression = RacketMetaSyntax.parseExpression(input)
  def equiv(equivalences: List[RacketEquivalence], a: RacketMetaExpression, b: RacketMetaExpression) : Boolean =
    EquivalenceSubstitution.normalizeExpr(a, b, equivalences).isDefined
  def rewrites(e: RacketMetaExpression, equiv: RacketEquivalence) : Iterable[RacketRewrite] =
    EquivalenceSubstitution.possibleRewrites(e, equiv)
  def possibleRewrite(equiv: RacketEquivalence, e: RacketMetaExpression, rewrite: RacketMetaExpression) : Boolean =
    rewrites(e, equiv).exists(_.toExpr == rewrite)
}

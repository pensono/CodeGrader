package com.ethanshea.codegrader

object EquivalenceSubstitution {
  // Equivalence parameter type could probably be
  def possibleRewrites(expr: RacketMetaExpression, racketEquivalences: List[RacketEquivalence]) = {
  }

  // Equivalence parameter type could probably be generalized
  def normalizeExpr(expr: RacketMetaExpression, norm: RacketMetaExpression, racketEquivalences: RacketEquivalence) : RacketMetaExpression = {
    // Normalize sub expressions
    val subRewritesExpr = expr match {
      case SExpression(children) => SExpression(children.map(normalizeExpr(_, norm, racketEquivalences)))
      case _ => expr
    }

    // Find rewrites
    val bindings = racketEquivalences.members.flatMap(matchExpr(subRewritesExpr, _))
    val templates = racketEquivalences.members.filter(matchExpr(norm, _).isDefined)

    val rewrittenExpr =
      if (bindings.nonEmpty && templates.nonEmpty)
        rewriteExpr(templates.head, bindings.head)
      else
        subRewritesExpr

    // Rewrite sub expressions
    (rewrittenExpr, norm) match {
      case (SExpression(eChildren), SExpression(tChildren)) =>
        if (eChildren.size == tChildren.size)
          SExpression(eChildren.zip(tChildren).map { case (e, t) => normalizeExpr(e, t, racketEquivalences) })
        else
          rewrittenExpr
      case _ => rewrittenExpr
    }
  }

  def equivalent(expr: RacketMetaExpression, norm: RacketMetaExpression, racketEquivalences: RacketEquivalence) : Boolean =
    normalizeExpr(expr, norm, racketEquivalences) == norm

  /**
    * Assumes that the template already matches
    * @param template
    * @param bindings
    * @return
    */
  def rewriteExpr(template: RacketMetaExpression, bindings: Map[String, RacketMetaExpression]) : RacketMetaExpression =
    template match {
      case Literal(_) => template
      case Identifier(_) => template
      case MetaVariable(name) => bindings(name)
      case SExpression(tChildren) =>
        SExpression(tChildren.map(rewriteExpr(_, bindings)))
    }

  /**
    * Attempts to match an expression with a template
    * @param expr Input expression, should contain no meta variables
    * @param template Template to match against
    * @return None if a match is not made, a mapping of meta variable names to the expressions they bind to
    */
  def matchExpr(expr: RacketMetaExpression, template: RacketMetaExpression) : Option[Map[String, RacketMetaExpression]] =
    (expr, template) match {
      case (Literal(eVal), Literal(nVal)) => if (eVal == nVal) Some(Map.empty) else None
      case (Identifier(eId), Identifier(nId)) => if (eId == nId) Some(Map.empty) else None
      case (e, MetaVariable(nName)) => Some(Map(nName -> e))
      case (SExpression(eChildren), SExpression(nChildren)) =>
        if (eChildren.size != nChildren.size) {
          return None
        }
        eChildren.zip(nChildren)
          .map { case (e, n) => matchExpr(e, n) }
          .fold[Option[Map[String, RacketMetaExpression]]](Some(Map.empty)) {
            (acc, m) => acc.flatMap{a => m.flatMap(safeMapMerge(a, _)) }
        }
      case _ => None
    }

  /***
    * Returns the combination of a and b unless share a key with different values, in which case, None will be returned
    * @param a
    * @param b
    * @tparam V
    * @return
    */
  private def safeMapMerge[V](a: Map[String, V], b: Map[String, V]): Option[Map[String, V]] = { // I can't figure out how to generalize the key type
    val sharedKeys = a.keySet intersect b.keySet
    if (sharedKeys.forall(k => a(k) == b(k)))
      Some(a ++ b)
    else
      None
  }
}

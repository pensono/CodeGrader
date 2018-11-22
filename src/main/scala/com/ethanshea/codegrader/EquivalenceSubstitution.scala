package com.ethanshea.codegrader

object EquivalenceSubstitution {
  /**
    * Attempts to match an expression with a template
    * @param expr Input expression, should contain no meta variables
    * @param template Template to match against
    * @return None if a match is not made, a mapping of meta variable names to the expressions they bind to
    */
  def matchExpr(expr: RacketMetaExpression, template: RacketMetaExpression) : Option[Map[_ <: String, RacketMetaExpression]] =
    (expr, template) match {
      case (Literal(eVal), Literal(nVal)) => if (eVal == nVal) Some(Map.empty) else None
      case (Identifier(eId), Identifier(nId)) => if (eId == nId) Some(Map.empty) else None
      case (e, MetaVariable(nName)) => Some(Map(nName -> e))
      case (SExpression(eChildren), SExpression(nChildren)) => {
        if (eChildren.size != nChildren.size) {
          return None
        }
        eChildren.zip(nChildren)
          .map { case (e, n) => matchExpr(e, n) }
          .fold(Some(Map.empty)) { (acc, m) => acc.flatMap{a => m.map(a ++ _) } }
      }
      case _ => None
    }
}

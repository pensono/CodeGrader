package com.ethanshea.codegrader

import scala.collection.JavaConverters._

import org.antlr.v4.runtime.{CharStreams, CodePointCharStream, CommonTokenStream}

sealed abstract class RacketMetaExpression
case class SExpression(children: List[RacketMetaExpression]) extends RacketMetaExpression {
  override def toString: String = "(" + children.mkString(" ") + ")"
}
case class Identifier(id: String) extends RacketMetaExpression {
  override def toString: String = id
}
case class Literal(value: String) extends RacketMetaExpression {
  override def toString: String = value
}
case class MetaVariable(name: String) extends RacketMetaExpression {
  override def toString: String = "$" + name
}

object RacketMetaSyntax {

  private def preparse(stream: CodePointCharStream) = {
    val tokenizer = new RacketMetaLexer(stream)
    val tokens = new CommonTokenStream(tokenizer)
    new RacketMetaParser(tokens)
  }

  def parseExpression(input: String) : RacketMetaExpression = {
    val parser = preparse(CharStreams.fromString(input))
    toRacketExpression(parser.expression())
  }

  def toRacketExpression(context: RacketMetaParser.ExpressionContext): RacketMetaExpression =
    context match {
      case s: RacketMetaParser.SExpressionContext => SExpression(s.subexpr.asScala.map(e => toRacketExpression(e)).toList)
      case i: RacketMetaParser.IdentifierContext => Identifier(i.id.getText)
      case l: RacketMetaParser.LiteralContext => Literal(l.value.getText)
      case v: RacketMetaParser.MetaVariableContext => MetaVariable(v.name.getText)
    }
}

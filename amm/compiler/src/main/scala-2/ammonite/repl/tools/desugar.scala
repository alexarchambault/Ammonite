package ammonite.repl.tools

import sourcecode.Compat._
import scala.language.experimental.macros


class Desugared(s: String){
  override def toString() = s
}
object desugar{
  def transformer(c: Context)(expr: c.Expr[Any]): c.Expr[Desugared] = {
    import c.universe._
    c.Expr[Desugared](
      q"ammonite.repl.tools.desugar.desugarImpl(${c.universe.showCode(expr.tree)})"
    )
  }

  def apply(expr: Any): Desugared = macro transformer

  def desugarImpl(s: String)(implicit colors: ammonite.util.CodeColors): Desugared = {

    new Desugared(
      ammonite.repl.Highlighter.defaultHighlight(
        ammonite.compiler.Parsers.Splitter(_),
        s.toCharArray,
        colors.comment,
        colors.`type`,
        colors.literal,
        colors.keyword,
        fansi.Attr.Reset
      ).mkString
    )
  }
}
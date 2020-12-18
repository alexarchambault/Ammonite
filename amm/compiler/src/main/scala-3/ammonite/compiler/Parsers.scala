package ammonite.compiler

import java.util.Map

import ammonite.compiler.iface.{Compiler => _, Parser => IParser, _}
import ammonite.util.Util.entry

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts.{ctx, Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.util.SourceFile

import scala.collection.mutable

class Parsers extends IParser {

  // FIXME Get via Compiler?
  private lazy val initCtx: Context =
    (new ContextBase).initialCtx

  private def parseStats(using Context): List[untpd.Tree] = {
    val parser = new Parser(ctx.source)
    val stats = parser.blockStatSeq()
    parser.accept(Tokens.EOF)
    stats
  }

  /** Check if the input is incomplete.
   *
   *  This can be used in order to check if a newline can be inserted without
   *  having to evaluate the expression.
   */
  private def isComplete(sourceCode: String)(using Context): Boolean =
    val reporter = Compiler.newStoreReporter()
    val source   = SourceFile.virtual("<incomplete-handler>", sourceCode, maybeIncomplete = true)
    val unit     = CompilationUnit(source, mustExist = false)
    val localCtx = ctx.fresh
                      .setCompilationUnit(unit)
                      .setReporter(reporter)
    var needsMore = false
    reporter.withIncompleteHandler((_, _) => needsMore = true) {
      parseStats(using localCtx)
    }
    reporter.hasErrors || !needsMore

  def trySplit(code: String, ignoreIncomplete: Boolean, fileName: String): Array[String] = {

    given Context = initCtx
    val reporter = Compiler.newStoreReporter()
    val source   = SourceFile.virtual("<splitter>", code, maybeIncomplete = true)
    val unit     = CompilationUnit(source, mustExist = false)
    val localCtx = ctx.fresh
                      .setCompilationUnit(unit)
                      .setReporter(reporter)
    var needsMore = false
    val stats = reporter.withIncompleteHandler((_, _) => needsMore = true) {
      parseStats(using localCtx)
    }

    val nl = System.lineSeparator
    def errors = reporter
      .removeBufferedMessages
      .map(_.msg.toString)
      .mkString(nl)

    if (reporter.hasErrors)
      throw new IParser.SplitException(s"Error splitting script:$nl$errors")
    else if (needsMore)
      null
    else {
      val startIndices = stats.toArray.map(_.startPos(using localCtx).point)
      def startEndIndices = startIndices.iterator
        .zip(startIndices.iterator.drop(1) ++ Iterator(code.length))
      startEndIndices
        .map {
          case (start, end) =>
            code.substring(start, end)
        }
        .toArray
    }
  }

  def importHooks(statement: String): (String, Seq[ImportTree]) = {

    given Context = initCtx
    val reporter = Compiler.newStoreReporter()
    val source   = SourceFile.virtual("<import-hooks-parser>", statement, maybeIncomplete = true)
    val unit     = CompilationUnit(source, mustExist = false)
    val localCtx = ctx.fresh
                      .setCompilationUnit(unit)
                      .setReporter(reporter)
    var needsMore = false
    val stats = reporter.withIncompleteHandler((_, _) => needsMore = true) {
      parseStats(using localCtx)
    }

    if (reporter.hasErrors || needsMore)
      (statement, Nil)
    else {
      var updatedStatement = statement
      var importTrees = Array.newBuilder[ImportTree]
      stats.foreach {
        case i: untpd.Import =>
          i.expr match {
            case untpd.Ident(name) if name.decode.toString.startsWith("$") =>
              val start = i.startPos.point
              val end = i.endPos.point
              val updatedImport = updatedStatement.substring(start, end).takeWhile(_ != '.') + ".$"
              updatedStatement = updatedStatement.patch(start, updatedImport + (" ") * (end - start - updatedImport.length), end - start)
              importTrees += new ImportTree(Array(name.decode.toString) ++ i.selectors.map(_.imported.name.decode.toString), null, start, end)
            case _ =>
          }
        case _ =>
      }
      (updatedStatement, importTrees.result)
    }
  }

  def importHooks(
    source: CodeSource,
    statements: Array[Map.Entry[Integer, String]]
  ): IParser.ParsedImportHooks = {

    val (updatedStatements, trees) = statements.map { e =>
      val statement = e.getValue
      importHooks(statement)
    }.unzip

    new IParser.ParsedImportHooks(updatedStatements, trees.flatten)
  }

  def scriptBlocks(
    rawCode: String,
    fileName: String
  ): Array[IParser.ScriptBlock] =
    // TODO
    Array(
      new IParser.ScriptBlock(
        0,
        "",
        Array(entry(0: Integer, rawCode))
      )
    )

  def scriptBlocksWithStartIndices(
    rawCode: String,
    fileName: String
  ): Array[IParser.ScriptBlock] =
    // TODO
    Array(
      new IParser.ScriptBlock(
        0,
        "",
        Array(entry(0: Integer, rawCode))
      )
    )

  def defaultHighlight(
    buffer: Array[Char],
    commentReset: Long,
    commentApply: Long,
    typeReset: Long,
    typeApply: Long,
    literalReset: Long,
    literalApply: Long,
    keywordReset: Long,
    keywordApply: Long,
    resetReset: Long,
    resetApply: Long
  ): String = {
    val resetAttrs = fansi.FansiHelper.attr(resetReset, resetApply)
    val valDefAttrs = resetAttrs
    val annotationAttrs = resetAttrs
    new SyntaxHighlighting(
      resetAttrs,
      fansi.FansiHelper.attr(commentReset, commentApply),
      fansi.FansiHelper.attr(keywordReset, keywordApply),
      valDefAttrs,
      fansi.FansiHelper.attr(literalReset, literalApply),
      fansi.FansiHelper.attr(typeReset, typeApply),
      annotationAttrs,
    ).highlight(new String(buffer))(using initCtx)
  }
}

object Parsers extends Parsers

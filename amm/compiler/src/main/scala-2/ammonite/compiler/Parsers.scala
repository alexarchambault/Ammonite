package ammonite.compiler

import java.util.Map

import ammonite.compiler.iface.{Parser => IParser, _}
import ammonite.util.InterfaceExtensions.importTree
import ammonite.util.Util.{entry, newLine, windowsPlatform}

import scala.collection.mutable

class Parsers extends IParser {

  import fastparse._

  import ScalaWhitespace._
  import scalaparse.Scala._

  // For some reason Scala doesn't import this by default
  private def `_`[_: P] = scalaparse.Scala.`_`


  private def ImportSplitter[_: P]: P[Seq[ImportTree]] = {
    def IdParser = P( (Id | `_` ).! ).map(
      s => if (s(0) == '`') s.drop(1).dropRight(1) else s
    )
    def Selector = P( IdParser ~ (`=>` ~/ IdParser).? )
    def Selectors = P( "{" ~/ Selector.rep(sep = ","./) ~ "}" )
    def BulkImport = P( `_`).map(
      _ => Seq("_" -> None)
    )
    def Prefix = P( IdParser.rep(1, sep = ".") )
    def Suffix = P( "." ~/ (BulkImport | Selectors) )
    def ImportExpr: P[ImportTree] = {
      // Manually use `WL0` parser here, instead of relying on WhitespaceApi, as
      // we do not want the whitespace to be consumed even if the WL0 parser parses
      // to the end of the input (which is the default behavior for WhitespaceApi)
      P( Index ~~ Prefix ~~ (WL0 ~~ Suffix).? ~~ Index).map{
        case (start, idSeq, selectors, end) =>
          importTree(idSeq, selectors, start, end)
      }
    }
    P( `import` ~/ ImportExpr.rep(1, sep = ","./) )
  }

  private def PatVarSplitter[_: P] = {
    def Prefixes = P(Prelude ~ (`var` | `val`))
    def Lhs = P( Prefixes ~/ BindPattern.rep(1, "," ~/ Pass) ~ (`:` ~/ Type).? )
    P( Lhs.! ~ (`=` ~/ WL ~ StatCtx.Expr.!) ~ End )
  }
  private def patVarSplit(code: String) = {
    val Parsed.Success((lhs, rhs), _) = parse(code, PatVarSplitter(_))
    (lhs, rhs)
  }

  private def Prelude[_: P] = P( (Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep )

  private def TmplStat[_: P] = P( Import | Prelude ~ BlockDef | StatCtx.Expr )


  // Do this funny ~~WS thing to make sure we capture the whitespace
  // together with each statement; otherwise, by default, it gets discarded.
  //
  // After each statement, there must either be `Semis`, a "}" marking the
  // end of the block, or the `End` of the input
  private def StatementBlock[_: P](blockSep: => P0) =
    P( Semis.? ~ (Index ~ (!blockSep ~ TmplStat ~~ WS ~~ (Semis | &("}") | End)).!).repX)

  private def Splitter0[_: P] = P( StatementBlock(Fail) )
  def Splitter[_: P] = P( ("{" ~ Splitter0 ~ "}" | Splitter0) ~ End )

  private def ObjParser[_: P] = P( ObjDef )

  /**
   * Attempts to break a code blob into multiple statements. Returns `None` if
   * it thinks the code blob is "incomplete" and requires more input
   */
  private def split(
    code: String,
    ignoreIncomplete: Boolean,
    fileName: String
  ): Option[Either[String, Seq[String]]] =
    if (ignoreIncomplete) {
      // We use `instrument` to detect when the parser has reached the end of the
      // input, any time during the parse. If it has done so, and failed, we
      // consider the input incomplete.
      var furthest = 0
      val instrument = new fastparse.internal.Instrument {
        def beforeParse(parser: String, index: Int): Unit = ()
        def afterParse(parser: String, index: Int, success: Boolean): Unit = {
          if (index > furthest) furthest = index
        }
      }

      parse(code, Splitter(_), instrument = instrument) match{
        case Parsed.Failure(_, index, extra) if furthest == code.length => None
        case f @ Parsed.Failure(_, _, _) => Some(Left(
          formatFastparseError(fileName, code, f)
        ))
        case Parsed.Success(value, index) => Some(Right(value.map(_._2)))
      }
    } else
      parse(code, Splitter(_)) match{
        case f @ Parsed.Failure(_, _, _) => Some(Left(
          formatFastparseError(fileName, code, f)
        ))
        case Parsed.Success(value, index) => Some(Right(value.map(_._2)))
      }

  def trySplit(code: String, ignoreIncomplete: Boolean, fileName: String): Array[String] =
    split(code, ignoreIncomplete, fileName) match {
      case None => null
      case Some(Left(error)) => throw new IParser.SplitException(error)
      case Some(Right(stmts)) => stmts.toArray
    }

  def isObjDef(code: String): Boolean = {
    parse(code, ObjParser(_))
      .fold((_, _, _) => false, (_, _) => true)
  }

  private def Separator[_: P] = P( WL ~ "@" ~~ CharIn(" \n\r").rep(1) )
  private def CompilationUnit[_: P] = P( WL.! ~ StatementBlock(Separator) ~ WL )

  private def ScriptSplitter[_: P] = P( CompilationUnit.repX(1, Separator) ~ End)
  private def splitScript(code: String): Parsed[Seq[(String, Seq[(Int, String)])]] =
    parse(code, ScriptSplitter(_))

  def formatFastparseError(fileName: String, rawCode: String, f: Parsed.Failure): String = {

    val lineColIndex = f.extra.input.prettyIndex(f.index)
    val expected = f.trace().failure.label
      val locationString = {
        val (first, last) = rawCode.splitAt(f.index)
        val lastSnippet = last.split(newLine).headOption.getOrElse("")
        val firstSnippet = first.reverse
          .split(newLine.reverse)
          .lift(0).getOrElse("").reverse
        firstSnippet + lastSnippet + newLine + (" " * firstSnippet.length) + "^"
      }
    s"$fileName:$lineColIndex expected $expected$newLine$locationString"
  }

  /**
    * Splits up a script file into its constituent blocks, each of which
    * is a tuple of (leading-whitespace, statements). Leading whitespace
    * is returned separately so we can later manipulate the statements e.g.
    * by adding `val res2 = ` without the whitespace getting in the way
    */
  def splitScript(
    rawCode: String,
    fileName: String
  ): Either[String, IndexedSeq[(String, Seq[String])]] = {
    parse(rawCode, ScriptSplitter(_)) match {
      case f: Parsed.Failure =>
        Left(formatFastparseError(fileName, rawCode, f))

      case s: Parsed.Success[Seq[(String, Seq[(Int, String)])]] =>

        var offset = 0
        val blocks = mutable.ArrayBuffer[(String, Seq[String])]()

        // comment holds comments or empty lines above the code which is not caught along with code
        for( (comment, codeWithStartIdx) <- s.value){
          val code = codeWithStartIdx.map(_._2)

          //ncomment has required number of newLines appended based on OS and offset
          //since fastparse has hardcoded `\n`s, while parsing strings with `\r\n`s it
          //gives out one extra `\r` after '@' i.e. block change
          //which needs to be removed to get correct line number (It adds up one extra line)
          //thats why the `comment.substring(1)` thing is necessary
          val ncomment =
            if(windowsPlatform && blocks.nonEmpty && !comment.isEmpty){
              comment.substring(1) + newLine * offset
            }else{
              comment + newLine * offset
            }

          // 1 is added as Separator parser eats up the newLine char following @
          offset = offset + (comment.split(newLine, -1).length - 1) +
            code.map(_.split(newLine, -1).length - 1).sum + 1
          blocks.append((ncomment, code))
        }

        Right(blocks.toIndexedSeq)
    }
  }


  def scriptBlocks(
    rawCode: String,
    fileName: String
  ): Array[ammonite.compiler.iface.Parser.ScriptBlock] =
    splitScript(rawCode, fileName) match {
      case Left(error) => throw new ammonite.compiler.iface.Parser.ScriptSplittingError(error)
      case Right(blocks) =>
        blocks
          .map {
            case (ncomment, code) =>
              val codeWithDummyIndices = code.map { content =>
                entry(0: Integer, content)
              }
              new ammonite.compiler.iface.Parser.ScriptBlock(ncomment, codeWithDummyIndices.toArray)
          }
          .toArray
    }

  def splitScriptWithStart(
    rawCode: String,
    fileName: String
  ): Either[Parsed.Failure, IndexedSeq[(Int, String, Seq[(Int, String)])]] =
    splitScriptWithStart(rawCode) match {
      case f: Parsed.Failure =>
        Left(f)

      case Parsed.Success(value, _) =>
        val blocks = value.toVector.map {
          case (startIdx, (comment, code)) =>
            (startIdx, comment, code)
        }
        Right(blocks)
    }

  def scriptBlocksWithStartIndices(
    rawCode: String,
    fileName: String
  ): Array[ammonite.compiler.iface.Parser.ScriptBlock] = {
    splitScriptWithStart(rawCode, fileName) match {
      case Left(f) =>
        throw new ammonite.compiler.iface.Parser.ScriptSplittingError(
          formatFastparseError(fileName, rawCode, f),
          f.index,
          f.trace().failure.label
        )
      case Right(blocks) =>
        blocks
          .map {
            case (startIdx, ncomment, code) =>
              val codeAsEntries = code.map {
                case (index, content) =>
                  entry(index: Integer, content)
              }
              new ammonite.compiler.iface.Parser.ScriptBlock(ncomment, codeAsEntries.toArray)
          }
          .toArray
    }
  }


  private def ScriptSplitterWithStart[_: P] =
    P( Start ~ (Index ~ CompilationUnit).repX(1, Separator) ~ End)
  private def splitScriptWithStart(code: String): Parsed[Seq[(Int, (String, Seq[(Int, String)]))]] =
    parse(code, ScriptSplitterWithStart(_))

  def stringWrap(s: String): String = "\"" + pprint.Util.literalize(s) + "\""
  def stringSymWrap(s: String): String = {
    def idToEnd[_: P] = P( scalaparse.syntax.Identifiers.Id ~ End )
    if (s == "") "'"
    else parse(s, idToEnd(_))  match{
      case Parsed.Success(v, _) =>  "'" + s
      case f: Parsed.Failure => stringWrap(s)
    }
  }
  def parseImportHooks(source: CodeSource, stmts: Seq[String]): (Seq[String], Seq[ImportTree]) =
    parseImportHooksWithIndices(source, stmts.map((0, _)))
  def parseImportHooksWithIndices(
    source: CodeSource,
    stmts: Seq[(Int, String)]
  ): (Seq[String], Seq[ImportTree]) = synchronized{
    val hookedStmts = mutable.Buffer.empty[String]
    val importTrees = mutable.Buffer.empty[ImportTree]
    for((startIdx, stmt) <- stmts) {
      // Call `fastparse.ParserInput.fromString` explicitly, to avoid generating a
      // lambda in the class body and making the we-do-not-load-fastparse-on-cached-scripts
      // test fail
      parse(fastparse.ParserInput.fromString(stmt), ImportSplitter(_)) match{
        case f: Parsed.Failure => hookedStmts.append(stmt)
        case Parsed.Success(parsedTrees, _) =>
          var currentStmt = stmt
          for(importTree <- parsedTrees){
            if (importTree.prefix.apply(0)(0) == '$') {
              val length = importTree.end - importTree.start
              currentStmt = currentStmt.patch(
                importTree.start, (importTree.prefix.apply(0) + ".$").padTo(length, ' '), length
              )
              val importTree0 = importTree.updateBounds(
                startIdx + importTree.start,
                startIdx + importTree.end
              )
              importTrees.append(importTree0)
            }
          }
          hookedStmts.append(currentStmt)
      }
    }
    (hookedStmts.toSeq, importTrees.toSeq)
  }

  def importHooks(
    source: CodeSource,
    statements: Array[Map.Entry[Integer, String]]
  ): IParser.ParsedImportHooks = {
    val (hookStatements, importTrees) = parseImportHooksWithIndices(
      source,
      statements.map(e => (e.getKey: Int, e.getValue))
    )
    new IParser.ParsedImportHooks(hookStatements.toArray, importTrees.toArray)
  }

  def defaultHighlight(buffer: Array[Char],
                       commentReset: Long,
                       commentApply: Long,
                       typeReset: Long,
                       typeApply: Long,
                       literalReset: Long,
                       literalApply: Long,
                       keywordReset: Long,
                       keywordApply: Long,
                       resetReset: Long,
                       resetApply: Long): String =
    ammonite.repl.Highlighter.defaultHighlight(
      Splitter(_),
      buffer,
      commentReset,
      commentApply,
      typeReset,
      typeApply,
      literalReset,
      literalApply,
      keywordReset,
      keywordApply,
      resetReset,
      resetApply
    )
}

object Parsers extends Parsers

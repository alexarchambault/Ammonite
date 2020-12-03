package ammonite.compiler

import ammonite.util._
import ammonite.util.InterfaceExtensions._
import ammonite.util.Util.{newLine, windowsPlatform}
import fastparse._

import scala.tools.nsc.{Global => G}
import collection.mutable

object Preprocessor{

  def formatFastparseError(fileName: String, rawCode: String, f: Parsed.Failure) = {

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
    Parsers.splitScript(rawCode) match {
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

  def splitScriptWithStart(
    rawCode: String,
    fileName: String
  ): Either[Parsed.Failure, IndexedSeq[(Int, String, Seq[(Int, String)])]] = {
    Parsers.splitScriptWithStart(rawCode) match {
      case f: Parsed.Failure =>
        Left(f)

      case Parsed.Success(value, _) =>
        val blocks = value.toVector.map {
          case (startIdx, (comment, code)) =>
            (startIdx, comment, code)
        }
        Right(blocks)
    }
  }
}
package ammonite.interp.api.doc

import java.util.function.Function

import scala.collection.mutable

trait InterpAPI extends Any with InterpCompilerAPI {
  /**
   * A generalization of [[watch]], allows watching arbitrary values and not
   * just the contents of file paths.
   */
  def watchValue[T](v: => T): T

  /**
   * resolvers to use when loading jars
   */
  def repositories: mutable.Buffer[coursierapi.Repository]

  /**
    * Functions that will be chained and called on the coursier
    * Fetch object right before they are run
    */
  def resolutionHooks: mutable.Buffer[Function[coursierapi.Fetch, coursierapi.Fetch]]

  /**
    * Functions that will be chained and called on the
    * exitValue before the repl exits
    */
  def beforeExitHooks: mutable.Buffer[Function[Object, Object]]
}
package ammonite.repl

import ammonite.repl.api.ReplAPI

import scala.reflect.runtime.universe._

trait ReplAPICompilerExtensions extends Any {

  protected def api: ReplAPI

  /**
   * Get the `Type` object of [[T]]. Useful for finding
   * what its methods are and what you can do with it
   */
  def typeOf[T: WeakTypeTag]: Type = scala.reflect.runtime.universe.weakTypeOf[T]
  /**
   * Get the `Type` object representing the type of `t`. Useful
   * for finding what its methods are and what you can do with it
   *
   */
  def typeOf[T: WeakTypeTag](t: => T): Type = scala.reflect.runtime.universe.weakTypeOf[T]

  /**
   * Access the compiler to do crazy things if you really want to!
   */
  def compiler: scala.tools.nsc.Global =
    api.objCompiler.asInstanceOf[scala.tools.nsc.Global]

  /**
    * Access the presentation compiler to do even crazier things if you really want to!
    */
  def interactiveCompiler: scala.tools.nsc.interactive.Global =
    api.objPressy.asInstanceOf[scala.tools.nsc.interactive.Global]
}

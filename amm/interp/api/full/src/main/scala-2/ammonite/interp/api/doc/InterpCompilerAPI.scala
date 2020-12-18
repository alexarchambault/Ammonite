package ammonite.interp.api.doc

trait InterpCompilerAPI extends Any {
  /**
    * Configures the current compiler, or if the compiler hasn't been initialized
    * yet, registers the configuration callback and applies it to the compiler
    * when it ends up being initialized later
    */
  def configureCompiler(c: scala.tools.nsc.Global => Unit): Unit

  /**
     * Pre-configures the next compiler. Useful for tuning options that are
     * used during parsing such as -Yrangepos
     */
  def preConfigureCompiler(c: scala.tools.nsc.Settings => Unit): Unit
}

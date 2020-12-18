package ammonite.interp.api

trait InterpAPICompilerExtensions extends Any with ammonite.interp.api.doc.InterpCompilerAPI {
  protected def api: InterpAPI

  def configureCompiler(c: scala.tools.nsc.Global => Unit): Unit =
    api
      .objCompilerLifeCycleManager
      .asInstanceOf[ammonite.compiler.CompilerLifecycleManager]
      .configureCompiler(c)
  def preConfigureCompiler(c: scala.tools.nsc.Settings => Unit): Unit =
    api
      .objCompilerLifeCycleManager
      .asInstanceOf[ammonite.compiler.CompilerLifecycleManager]
      .preConfigureCompiler(c)
}

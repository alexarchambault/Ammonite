package ammonite.interp.api

class ScriptInvokerImpl extends ScriptInvoker {

  def invoke(
    parser: Object,
    scriptName: String,
    scriptArgs: Array[String]
  ): ScriptInvoker.Result =
    // new ScriptInvoker.Result.Failure("Not supported in Scala 3")
    new ScriptInvoker.Result.Success(())

  def helpText(parser: Object, totalWidth: Int, docsOnNewLine: Boolean): String =
    "Entrypoints not supported in Scala 3"

}

object ScriptInvokerImpl extends ScriptInvokerImpl

package ammonite.compiler

import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.io.OutputStream

import ammonite.compiler.iface.{Compiler => ICompiler, Preprocessor => IPreprocessor, _}
import ammonite.util.InterfaceExtensions._
import ammonite.util.Util.entry

import dotty.tools.dotc.{CompilationUnit, Compiler => DottyCompiler, Run}
import dotty.tools.dotc.ast.Positioned
import dotty.tools.dotc.classpath.{AggregateClassPath, ClassPathFactory, JrtClassPath, ZipAndJarClassPathFactory}
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.dotc.config.{JavaPlatform, PathResolver} // DEBUG
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.{MacroClassLoader, Mode}
import dotty.tools.dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotty.tools.dotc.core.Phases.{Phase, unfusedPhases}
import dotty.tools.dotc.fromtasty.TastyFileUtil
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.{Diagnostic, HideNonSensicalMessages, Message, MessageRendering, StoreReporter, UniqueMessagePositions}
import dotty.tools.dotc.transform.{PostTyper, Staging}
import dotty.tools.dotc.typer.FrontEnd
import dotty.tools.dotc.util.{Property, SourceFile, SourcePosition}
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation, File, VirtualDirectory}
import dotty.tools.repl.CollectTopLevelImports

class Compiler(
  outputDir: VirtualDirectory,
  initialClassPath: Seq[URL],
  classPath: Seq[URL],
  macroClassLoader: ClassLoader,
  whiteList: Set[Seq[String]]
) extends ICompiler:
  self =>

  private def initCtx: Context =
    val base: ContextBase =
      new ContextBase:
        override protected def newPlatform(using Context) =
          new JavaPlatform:
            private var classPath0: ClassPath = null
            override def classPath(using Context) =
              if (classPath0 == null)
                classPath0 = AggregateClassPath(Seq(
                  asDottyClassPath(initialClassPath, whiteListed = true),
                  asDottyClassPath(self.classPath)
                ))
              classPath0
    base.initialCtx

  private def settings: Array[String] = Array()

  private def sourcesRequired = false

  private lazy val MacroClassLoaderKey =
    val cls = macroClassLoader.loadClass("dotty.tools.dotc.core.MacroClassLoader$")
    val fld = cls.getDeclaredField("MacroClassLoaderKey")
    fld.setAccessible(true)
    fld.get(null).asInstanceOf[Property.Key[ClassLoader]]

  private def setup(args: Array[String], rootCtx: Context): (List[String], Context) =
    val ictx = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(using ictx)
    ictx.setSettings(summary.sstate)
    ictx.setProperty(MacroClassLoaderKey, macroClassLoader)
    Positioned.init(using ictx)

    inContext(ictx) {
      if !ctx.settings.YdropComments.value || ctx.mode.is(Mode.ReadComments) then
        ictx.setProperty(ContextDoc, new ContextDocstrings)
      val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)
      fromTastySetup(fileNames, ctx)
    }

  private def asDottyClassPath(cp: Seq[URL], whiteListed: Boolean = false)(using Context): ClassPath =
    val (dirs, jars) = cp.partition { url =>
      url.getProtocol == "file" && Files.isDirectory(Paths.get(url.toURI))
    }

    val dirsCp = dirs.map(u => ClassPathFactory.newClassPath(AbstractFile.getURL(u)))
    val jarsCp = jars
      .filter(ammonite.util.Classpath.canBeOpenedAsJar)
      .map(u => ZipAndJarClassPathFactory.create(AbstractFile.getURL(u)))

    if (whiteListed) new dotty.ammonite.compiler.WhiteListClasspath(dirsCp ++ jarsCp, whiteList)
    else AggregateClassPath(dirsCp ++ jarsCp)

  /** Setup extra classpath and figure out class names for tasty file inputs */
  private def fromTastySetup(fileNames0: List[String], ctx0: Context): (List[String], Context) =
    given Context = ctx0
    if (ctx0.settings.fromTasty.value)
      val fromTastyIgnoreList = ctx0.settings.YfromTastyIgnoreList.value.toSet
      // Resolve classpath and class names of tasty files
      val (classPaths, classNames) = fileNames0.flatMap { name =>
        val path = Paths.get(name)
        if !Files.exists(path) then
          report.error(s"File does not exist: $name")
          Nil
        else if name.endsWith(".jar") then
          new dotty.tools.io.Jar(File(name)).toList.collect {
            case e if e.getName.endsWith(".tasty") && !fromTastyIgnoreList(e.getName) =>
              (name, e.getName.stripSuffix(".tasty").replace("/", "."))
          }
        else if name.endsWith(".tasty") then
          TastyFileUtil.getClassName(path) match
            case Some(res) => res :: Nil
            case _ =>
              report.error(s"Could not load classname from: $name")
              Nil
        else
          report.error(s"File extension is not `tasty` or `jar`: $name")
          Nil
      }.unzip
      val ctx1 = ctx0.fresh
      val classPaths1 = classPaths.distinct.filter(_ != "")
      val fullClassPath = (classPaths1 :+ ctx1.settings.classpath.value(using ctx1)).mkString(java.io.File.pathSeparator)
      ctx1.setSetting(ctx1.settings.classpath, fullClassPath)
      (classNames, ctx1)
    else
      (fileNames0, ctx0)

  /** Create a fresh and initialized context with IDE mode enabled */
  private lazy val initialCtx =
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive | Mode.ReadComments)
    rootCtx.setSetting(rootCtx.settings.YcookComments, true)
    // FIXME Disabled for the tests to pass
    rootCtx.setSetting(rootCtx.settings.color, "never")
    // FIXME We lose possible custom openStream implementations on the URLs of initialClassPath and
    // classPath
    val initialClassPath0 = initialClassPath
      // .filter(!_.toURI.toASCIIString.contains("fansi_2.13"))
      // .filter(!_.toURI.toASCIIString.contains("pprint_2.13"))
    rootCtx.setSetting(rootCtx.settings.classpath, (initialClassPath0 ++ classPath).map(_.toURI.toASCIIString).mkString(java.io.File.pathSeparator))
    rootCtx.setSetting(rootCtx.settings.outputDir, outputDir)

    val (_, ictx) = setup(settings, rootCtx)
    ictx.base.initialize()(using ictx)
    ictx

  private def enumerateVdFiles(d: VirtualDirectory): Iterator[AbstractFile] =
    val (subs, files) = d.iterator.partition(_.isDirectory)
    files ++ subs.map(_.asInstanceOf[VirtualDirectory]).flatMap(enumerateVdFiles)

  private def files(d: VirtualDirectory): Iterator[(String, Array[Byte])] =
    for (x <- enumerateVdFiles(d) if x.name.endsWith(".class")) yield {
      val segments = x.path.split("/").toList.tail
      val output = writeDeep(d, segments, "")
      output.write(x.toByteArray)
      output.close()
      (x.path.stripPrefix("(memory)/").stripSuffix(".class").replace('/', '.'), x.toByteArray)
    }

  private def writeDeep(
    d: VirtualDirectory,
    path: List[String],
    suffix: String
  ): OutputStream = path match {
    case head :: Nil => d.fileNamed(path.head + suffix).output
    case head :: rest =>
      writeDeep(
        d.subdirectoryNamed(head).asInstanceOf[VirtualDirectory],
        rest, suffix
      )
    // We should never write to an empty path, and one of the above cases
    // should catch this and return before getting here
    case Nil => ???
  }

  private var userCodeNestingLevel = -1

  val compiler =
    new DottyCompiler:
      override protected def frontendPhases: List[List[Phase]] = List(
        List(new FrontEnd), // List(new REPLFrontEnd),
        List(new AmmonitePhase(userCodeNestingLevel, userCodeNestingLevel == 2)),
        List(new Staging),
        List(new PostTyper)
      )

  /** A `MessageRenderer` for the REPL without file positions */
  private val messageRenderer =
    new MessageRendering:
      override def sourceLines(pos: SourcePosition, diagnosticLevel: String)(using Context): (List[String], List[String], Int) = {
        val (srcBefore, srcAfter, offset) = super.sourceLines(pos, diagnosticLevel)
        val updatedSrcBefore = srcBefore.map { line =>
          val chars = line.toCharArray
          var i = 0
          var updated = false
          while (i < chars.length) {
            if (chars(i) == '|')
              i = chars.length
             else if (chars(i).isDigit) {
               chars(i) = ' '
               updated = true
             }
            i += 1
          }
          if (updated) new String(chars)
          else line
        }
        (updatedSrcBefore, srcAfter, offset)
      }
      override def posStr(pos: SourcePosition, diagnosticLevel: String, message: Message)(using Context): String = ""

  /** Formats errors using the `messageRenderer` */
  private def formatError(dia: Diagnostic)(implicit ctx: Context): Diagnostic =
    new Diagnostic(
      messageRenderer.messageAndPos(dia.msg, dia.pos, messageRenderer.diagnosticLevel(dia)),
      dia.pos,
      dia.level
    )

  def compileOrNull(
    src: Array[Byte],
    printer: Logger,
    importsLen: Int,
    userCodeNestingLevel: Int,
    fileName: String
  ): ICompiler.Output =
    val sourceFile = SourceFile.virtual(fileName, new String(src, StandardCharsets.UTF_8))
    // println(s"Compiling\n${new String(src, StandardCharsets.UTF_8)}\n")

    self.userCodeNestingLevel = userCodeNestingLevel

    val reporter = Compiler.newStoreReporter()
    val run = new Run(compiler, initialCtx.fresh.setReporter(reporter))
    implicit val ctx: Context = run.runContext.withSource(sourceFile)

    val unit =
      new CompilationUnit(ctx.source):
        override def isSuspendable: Boolean = false
    ctx //.fresh.setCompilationUnit(unit)
      .run
      .compileUnits(unit :: Nil)

    val result =
      if (ctx.reporter.hasErrors) Left(ctx.reporter.removeBufferedMessages)
      else Right(unit)

    result match {
      case Left(errors) =>
        errors
          .map(formatError)
          .map(_.msg.toString)
          .foreach(printer.printError)
        null
      case Right(unit) =>
        val newImports = unfusedPhases.collectFirst {
          case p: AmmonitePhase => p.importData
        }.getOrElse(Array.empty[Imports.Data])
        val usedEarlierDefinitions = unfusedPhases.collectFirst {
          case p: AmmonitePhase => p.usedEarlierDefinitions.toArray
        }.getOrElse(Array.empty[String])
        val fileCount = enumerateVdFiles(outputDir).length
        // println(s"Found ${fileCount} files:")
        // enumerateVdFiles(outputDir).map("  " + _).foreach(println)
        val classes = files(outputDir).toArray // .subdirectoryNamed("<REPL compilation output>").asInstanceOf[VirtualDirectory]
        // println(s"Found ${classes.length} classes")
        new ICompiler.Output(
          classes.map { case (n, b) =>
            // println(s"class $n")
            entry(n, b)
          },
          new Imports(newImports),
          usedEarlierDefinitions
        )
    }

  def objCompiler = compiler

  def preprocessor(fileName: String, markGeneratedSections: Boolean): IPreprocessor =
    new Preprocessor(
      initialCtx.fresh.withSource(SourceFile.virtual(fileName, "")),
      markGeneratedSections: Boolean
    )

object Compiler:

  /** Create empty outer store reporter */
  def newStoreReporter(): StoreReporter =
    new StoreReporter(null)
    with UniqueMessagePositions with HideNonSensicalMessages

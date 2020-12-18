import mill._, scalalib._, publish._
import ammonite.ops._, ImplicitWd._
import $file.ci.upload

import $ivy.`io.get-coursier::coursier-launcher:2.0.0-RC6-10`
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

val isMasterCommit =
  sys.env.get("GITHUB_REPOSITORY") == Some("lihaoyi/Ammonite") &&
  sys.env.get("GITHUB_REF").exists(x => x.endsWith("/master"))

val latestTaggedVersion = os.proc('git, 'describe, "--abbrev=0", "--tags").call().out.trim

val gitHead = os.proc('git, "rev-parse", "HEAD").call().out.trim

val commitsSinceTaggedVersion = {
  os.proc('git, "rev-list", gitHead, "--not", latestTaggedVersion, "--count")
    .call()
    .out
    .trim
    .toInt
}


val scala3Version = "3.0.0-M2"
val binCrossScalaVersions = Seq("2.12.12", "2.13.3")
def isScala2_12_10OrLater(sv: String): Boolean = {
  (sv.startsWith("2.12.") && sv.stripPrefix("2.12.").length > 1) || (sv.startsWith("2.13.") && sv != "2.13.0")
}
val fullCrossScalaVersions = binCrossScalaVersions /*Seq(
  "2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.6", "2.12.7", "2.12.8", "2.12.9", "2.12.10", "2.12.11", "2.12.12",
  "2.13.0", "2.13.1", "2.13.2", "2.13.3"
)*/

lazy val latestAssemblies = binCrossScalaVersions.map(amm(_).assembly)

println("GITHUB REF " + sys.env.get("GITHUB_REF"))

val (buildVersion, unstable) = scala.util.Try(
  os.proc('git, 'describe, "--exact-match", "--tags", "--always", gitHead)
    .call()
    .out
    .trim
).toOption match{
  case None =>
    val gitHash = os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim
    (s"$latestTaggedVersion-$commitsSinceTaggedVersion-$gitHash", true)
  case Some(tagName) => (tagName, false)
}

val bspVersion = "2.0.0-M6"

trait AmmInternalJavaModule extends JavaModule {
  def artifactName = "ammonite-" + millOuterCtx.segments.parts.mkString("-").stripPrefix("amm-")
  def externalSources = T{
    resolveDeps(transitiveIvyDeps, sources = true)()
  }
}
trait AmmInternalModule extends mill.scalalib.CrossSbtModule{
  def artifactName = "ammonite-" + millOuterCtx.segments.parts.mkString("-").stripPrefix("amm-")
  def testFramework = "utest.runner.Framework"

  def isScala2 = crossScalaVersion.startsWith("2.")
  def scalacOptions = {
    if (isScala2) Seq("-P:acyclic:force")
    else Nil
  }
  def compileIvyDeps = {
    if (isScala2) Agg(ivy"com.lihaoyi::acyclic:0.2.0")
    else Agg[Dep]()
  }
  def scalacPluginIvyDeps = {
    if (isScala2) Agg(ivy"com.lihaoyi::acyclic:0.2.0")
    else Agg[Dep]()
  }
  trait Tests extends super.Tests{
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.3")
    def testFrameworks = Seq("utest.runner.Framework")
    def forkArgs = Seq("-Xmx2g", "-Dfile.encoding=UTF8")
  }
  def allIvyDeps = T{transitiveIvyDeps() ++ scalaLibraryIvyDeps()}
  def sources = T.sources{
    val sv = scalaVersion()
    val extraDir =
      if (sv.startsWith("2.12.")) {
        val patch = sv.stripPrefix("2.12.").takeWhile(_.isDigit).toInt
        val dirName =
          if (patch <= 8)
            "scala-2.12.0_8"
          else
            "scala-2.12.9+"
        Seq(PathRef(millSourcePath / "src" / "main" / dirName))
      } else
        Nil

    val extraDir2 =
      if (isScala2)
        Seq(PathRef(
          if (isScala2_12_10OrLater(sv)) millSourcePath / "src" / "main" / "scala-2.12.10-2.13.1+"
          else millSourcePath / "src" / "main" / "scala-not-2.12.10-2.13.1+"
        ))
      else Nil
    val extraDir3 =
      if (isScala2) {
        if (!sv.startsWith("2.13.") || sv == "2.13.0")
          Seq(PathRef(millSourcePath / "src" / "main" / "scala-not-2.13.1+"))
        else
          Seq(PathRef(millSourcePath / "src" / "main" / "scala-2.13.1+"))
      } else Nil
    val extraDir4 =
      if (sv.startsWith("2.13.") || sv.startsWith("3."))
        Seq(PathRef(millSourcePath / "src" / "main" / "scala-2.13-or-3"))
      else Nil

    super.sources() ++ extraDir ++ extraDir2 ++ extraDir3 ++ extraDir4
  }
  def externalSources = T{
    resolveDeps(allIvyDeps, sources = true)()
  }
}
trait AmmPublishModule extends PublishModule{
  def publishVersion = buildVersion
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/Ammonite",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("lihaoyi", "ammonite"),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  def transitiveJars: T[Agg[PathRef]] = T{
    mill.define.Task.traverse(this +: moduleDeps)(m =>
      T.task{m.jar()}
    )()
  }

  def transitiveSourceJars: T[Agg[PathRef]] = T{
    mill.define.Task.traverse(this +: moduleDeps)(m =>
      T.task{m.sourceJar()}
    )()
  }
}
trait AmmModule extends AmmInternalModule with AmmPublishModule
trait AmmDependenciesResourceFileModule extends JavaModule{
  def dependencyResourceFileName: String
  def dependencyFileResources = T{
    val deps0 = T.task{compileIvyDeps() ++ transitiveIvyDeps()}()
    val (_, res) = mill.modules.Jvm.resolveDependenciesMetadata(
      repositories,
      deps0.map(resolveCoursierDependency().apply(_)),
      deps0.filter(_.force).map(resolveCoursierDependency().apply(_)),
      mapDependencies = Some(mapDependencies())
    )


    Seq(PathRef(generateDependenciesFile(
      dependencyResourceFileName,
      res.minDependencies.toSeq
    )))
  }

  def resources = T.sources {
    super.resources() ++ dependencyFileResources()
  }
}

object ops extends Cross[OpsModule](binCrossScalaVersions:_*)
class OpsModule(val crossScalaVersion: String) extends AmmModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.7.1",
    ivy"org.scala-lang.modules::scala-collection-compat:2.3.1"
  )
  def scalacOptions = super.scalacOptions().filter(!_.contains("acyclic"))
  object test extends Tests
}

object terminal extends Cross[TerminalModule](binCrossScalaVersions:_*)
class TerminalModule(val crossScalaVersion: String) extends AmmModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.2.1",
    ivy"com.lihaoyi::fansi:0.2.8"
  )
  def compileIvyDeps = Agg(
    ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
  )

  object test extends Tests
}

object amm extends Cross[MainModule](binCrossScalaVersions:_*){
  object util extends Cross[UtilModule](binCrossScalaVersions ++ Seq(scala3Version):_*)
  class UtilModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(compiler.interface)
    def ivyDeps = Agg(
      ivy"com.lihaoyi::pprint:0.6.0",
      ivy"com.lihaoyi::fansi:0.2.9",
      ivy"com.lihaoyi::os-lib:0.7.1",
      ivy"org.scala-lang.modules::scala-collection-compat:2.3.1"
    )
    def compileIvyDeps = {
      if (crossScalaVersion.startsWith("2."))
        Agg(
          ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
        )
      else
        Agg[Dep]()
    }
  }

  object runtime extends Cross[RuntimeModule](binCrossScalaVersions:_*)
  class RuntimeModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(amm.util(), interp.api.full(), amm.repl.api.full())
    def ivyDeps = Agg(
      ivy"com.lihaoyi::upickle:1.2.0",
      ivy"com.lihaoyi::requests:0.6.5"
    )
  }

  object compiler extends Cross[CompilerModule](fullCrossScalaVersions ++ Seq(scala3Version):_*) {
    object interface extends JavaModule with AmmPublishModule with AmmInternalJavaModule {
      def artifactName = "ammonite-compiler-interface"
      def exposedClassPath = T{
        runClasspath() ++
          externalSources() ++
          transitiveJars() ++
          transitiveSourceJars()
      }
    }
  }
  class CompilerModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(amm.util())
    def crossFullScalaVersion = true
    def ivyDeps = {
      val scalaSpecificDeps =
        if (isScala2)
          Agg(
            ivy"org.scala-lang:scala-compiler:$crossScalaVersion",
            ivy"com.lihaoyi::scalaparse:2.3.0",
            ivy"com.lihaoyi::mainargs:0.1.4"
          )
        else
          Agg[Dep](
            ivy"org.scala-lang:scala3-compiler_$crossScalaVersion:$crossScalaVersion"
          )
        scalaSpecificDeps ++ Agg(
          ivy"org.javassist:javassist:3.21.0-GA",
          ivy"com.github.javaparser:javaparser-core:3.2.5",
          ivy"org.scala-lang.modules::scala-xml:2.0.0-M3"
        )
    }
  }

  object interp extends Cross[InterpModule](binCrossScalaVersions:_*){
    object api extends JavaModule with AmmPublishModule with AmmDependenciesResourceFileModule {
      def artifactName = "ammonite-interp-api"
      def dependencyResourceFileName = "amm-interp-api-dependencies.txt"
      def ivyDeps = Agg(
        ivy"io.get-coursier:interface:0.0.21"
      )
      object full extends Cross[InterpApiModule](fullCrossScalaVersions ++ Seq(scala3Version):_*)
      class InterpApiModule(val crossScalaVersion: String) extends AmmModule{
        def moduleDeps = Seq(api, amm.compiler(), amm.util())
        def crossFullScalaVersion = true
        def ivyDeps =
          if (isScala2)
            Agg(
              ivy"com.lihaoyi::mainargs:0.1.4"
            )
          else
            Agg[Dep]()
      }
    }
  }
  class InterpModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(ops(), amm.util(), amm.runtime())
    def ivyDeps = Agg(
      ivy"ch.epfl.scala:bsp4j:$bspVersion",
      ivy"org.scalameta::trees:4.3.20",
      ivy"org.scala-lang:scala-compiler:$crossScalaVersion",
      ivy"org.scala-lang:scala-reflect:$crossScalaVersion",
      ivy"com.lihaoyi::scalaparse:2.3.0",
      ivy"org.scala-lang.modules::scala-xml:2.0.0-M3"
    )
  }

//  object `test-runner` extends mill.scalalib.SbtModule {
//    def scalaVersion = "2.12.8"
//    def ivyDeps = Agg(
//      ivy"com.lihaoyi::mill-scalalib:${sys.props("MILL_VERSION")}"
//    )
//  }

  object repl extends Cross[ReplModule](binCrossScalaVersions:_*){

    object api extends JavaModule with AmmPublishModule with AmmInternalJavaModule {
      def artifactName = "ammonite-repl-api"
      def moduleDeps = Seq(
        interp.api,
        compiler.interface
      )
      def exposedClassPath = T{
        runClasspath() ++
          externalSources() ++
          transitiveJars() ++
          transitiveSourceJars()
      }
      object full extends Cross[ReplApiFullModule](fullCrossScalaVersions ++ Seq(scala3Version):_*)
      class ReplApiFullModule(val crossScalaVersion: String) extends AmmModule with AmmDependenciesResourceFileModule{
        def crossFullScalaVersion = true
        def moduleDeps = Seq(
          api,
          amm.util(),
          interp.api.full()
        )
        def dependencyResourceFileName = "amm-dependencies.txt"

        def generatedSources = T{
          Seq(PathRef(generateConstantsFile(buildVersion, bspVersion = bspVersion)))
        }

        def exposedClassPath = T{
          runClasspath() ++
            externalSources() ++
            transitiveJars() ++
            transitiveSourceJars()
        }
      }
    }
    object `test-bridge` extends Cross[TestBridgeModule](fullCrossScalaVersions ++ Seq(scala3Version):_*)
    class TestBridgeModule(val crossScalaVersion: String) extends AmmModule{
      def crossFullScalaVersion = true
      def moduleDeps = Seq(
        amm.repl.api.full()
      )
    }
    // not a "real" cross module
    // The cross parameter is the tested *userspace* Scala version.
    // The sources of cross-tests itself (which are the same as the tests of amm.repl)
    // are only compiled with the latest Scala version.
    object `cross-tests` extends Cross[CrossTestModule](fullCrossScalaVersions ++ Seq(scala3Version):_*)
    class CrossTestModule(val testScalaVersion: String) extends AmmModule {
      private def sv = binCrossScalaVersions.last
      def crossScalaVersion = sv
      def scalaVersion = sv
      def moduleDeps = Seq(
        amm.repl(sv)
      )


      object test extends Tests with AmmDependenciesResourceFileModule {
        def dependencyResourceFileName = "amm-test-dependencies.txt"

        def moduleDeps = super.moduleDeps ++ Seq(
          amm.repl.`test-bridge`(sv)
        )

        def thinWhitelist = T{
          generateApiWhitelist(
            amm.repl.api.exposedClassPath(),
            // This one lives in test-bridge.
            // It's a simple Java class, with no dependencies,
            // so we pull it manually, instead of putting it in a
            // dedicated Java module and adding that module here.
            Seq("ammonite/TestReplApi.class")
          )
        }
        def localClasspath = T{
          super.localClasspath() ++ Agg(thinWhitelist())
        }

        // Pass the class path of amm.compiler and amm.repl.api.full for
        // the *tested* Scala version, via some Java properties.
        def forkArgs = T{
          val classPath = amm.compiler(testScalaVersion).runClasspath() ++
            amm.repl.api.full(testScalaVersion).runClasspath() ++
            amm.repl.`test-bridge`(testScalaVersion).runClasspath() ++
            localClasspath()
          val classPathStr = classPath
            .map(_.path.toString)
            .mkString(java.io.File.pathSeparator)
          // hope we won't get a too long argument error on Windows
          super.forkArgs() ++ Seq(
            s"-Damm.cross-tests.side-jars=$classPathStr",
            s"-Damm.cross-tests.scala-version=$testScalaVersion"
          )
        }

        def resources = T.sources {
          super.resources() ++
            amm.repl(sv).test.resources()
        }
        def sources = T.sources {
          super.sources() ++
            amm.repl(sv).test.sources()
        }
      }
    }

  }
  class ReplModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(
      ops(), amm.util(),
      amm.runtime(), amm.interp(), amm.compiler(),
      terminal()
    )
    def ivyDeps = Agg(
      ivy"org.jline:jline-terminal:3.14.1",
      ivy"org.jline:jline-terminal-jna:3.14.1",
      ivy"org.jline:jline-reader:3.14.1",
//      ivy"com.github.scopt::scopt:3.7.1"
    )

    object test extends Tests with AmmDependenciesResourceFileModule {
      def crossScalaVersion = ReplModule.this.crossScalaVersion
      def scalaVersion = ReplModule.this.crossScalaVersion
      def dependencyResourceFileName = "amm-test-dependencies.txt"

      def moduleDeps = super.moduleDeps ++ Seq(
        amm.repl.`test-bridge`()
      )

      def thinWhitelist = T{
        generateApiWhitelist(
          amm.repl.api.full().exposedClassPath() ++
          Seq(compile().classes) ++
          resolveDeps(T.task{compileIvyDeps() ++ transitiveIvyDeps()})()
        )
      }

      def localClasspath = T{
        super.localClasspath() ++ Agg(thinWhitelist())
      }

      def resources = T.sources {
        (super.resources() ++
        ReplModule.this.sources() ++
        ReplModule.this.externalSources() ++
        resolveDeps(ivyDeps, sources = true)()).distinct
      }
      def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.scalaz::scalaz-core:7.2.27"
      )
    }
  }
  object cross extends Cross[CrossModule](fullCrossScalaVersions ++ Seq(scala3Version):_*)
  class CrossModule(val testScalaVersion: String) extends AmmInternalModule{
    private def sv = binCrossScalaVersions.last
    def crossScalaVersion = sv
    def scalaVersion = sv
    def moduleDeps = Seq(amm(sv))

    def mainClass = amm(sv).mainClass()

    // Pass the class path of amm.compiler and amm.repl.api.full for
    // the *tested* Scala version, via some Java properties.
    def forkArgs = if (testScalaVersion.isEmpty) T{super.forkArgs()} else T{
      val extraArgs = {
        val classPath = amm.compiler(testScalaVersion).runClasspath() ++
          amm.repl.api.full(testScalaVersion).runClasspath() ++
          localClasspath()
        val classPathStr = classPath
          .map(_.path.toString)
          .mkString(java.io.File.pathSeparator)
        // hope we won't get a too long argument error on Windows
        Seq(
          s"-Damm.side-jars=$classPathStr",
          s"-Damm.scala-version=$testScalaVersion"
        )
      }

      super.forkArgs() ++ extraArgs
    }
  }
}

class MainModule(val crossScalaVersion: String)
  extends AmmModule {

  def artifactName = "ammonite"

  def mainClass = Some("ammonite.Main")

  def moduleDeps = Seq(
    terminal(), ops(),
    amm.util(), amm.runtime(),
    amm.interp.api.full(),
    amm.repl.api.full(),
    amm.interp(), amm.repl()
  )

  def runClasspath =
    super.runClasspath() ++
    ops().sources() ++
    terminal().sources() ++
    amm.util().sources() ++
    amm.runtime().sources() ++
    amm.interp.api.full().sources() ++
    amm.repl.api.full().sources() ++
    amm.interp().sources() ++
    amm.repl().sources() ++
    sources() ++
    externalSources()

  def prependShellScript = T{
    mill.modules.Jvm.launcherUniversalScript(
      mainClass().get,
      Agg("$0"),
      Agg("%~dpnx0"),
      // G1 Garbage Collector is awesome https://github.com/lihaoyi/Ammonite/issues/216
      Seq("-Xmx500m", "-XX:+UseG1GC")
    )
  }

  def thinWhitelist = T{
    generateApiWhitelist(
      amm.repl.api.exposedClassPath()
    )
  }
  def localClasspath = T{
    super.localClasspath() ++ Agg(thinWhitelist())
  }

  def launcher = {
    val isWindows = scala.util.Properties.isWin
    if (isWindows)
      T{
        val mainClass = finalMainClass()
        val cp = runClasspath().map(_.path)
        val jvmOpts = forkArgs()
        val dest = T.ctx().dest / "run.bat"

        import coursier.launcher.{BootstrapGenerator, ClassLoaderContent, Parameters, Preamble}
        val classLoaderContent = ClassLoaderContent.fromUrls(cp.map(_.toNIO.toUri.toASCIIString))
        val params = Parameters.Bootstrap(Seq(classLoaderContent), mainClass)
          .withPreamble(
            Preamble()
              .withKind(Preamble.Kind.Bat)
              .withJavaOpts(jvmOpts)
          )
        val thread = Thread.currentThread()
        val cl = thread.getContextClassLoader
        try {
          thread.setContextClassLoader(BootstrapGenerator.getClass.getClassLoader)
          BootstrapGenerator.generate(params, dest.toNIO)
        } finally {
          thread.setContextClassLoader(cl)
        }

        PathRef(dest)
      }
    else
      T{
        super.launcher()
      }
  }

  object test extends Tests{
    def moduleDeps = super.moduleDeps ++ Seq(amm.repl().test)
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.chuusai::shapeless:2.3.3",
      ivy"org.scala-lang.modules::scala-java8-compat:0.9.0"
    )


    def thinWhitelist = T{
      generateApiWhitelist(
        amm.repl.api.full().exposedClassPath() ++
        Seq(amm.repl().test.compile().classes, compile().classes) ++
        resolveDeps(T.task{compileIvyDeps() ++ transitiveIvyDeps()})()
      )
    }

    def localClasspath = T{
      super.localClasspath() ++ Agg(thinWhitelist())
    }

    // Need to duplicate this from MainModule due to Mill not properly propagating it through
    def runClasspath =
      super.runClasspath() ++
      ops().sources() ++
      terminal().sources() ++
      amm.util().sources() ++
      amm.runtime().sources() ++
      amm.interp.api.full().sources() ++
      amm.repl.api.full().sources() ++
      amm.interp().sources() ++
      amm.repl().sources() ++
      sources() ++
      externalSources()

  }
}

def generateApiWhitelist(replApiCp: Seq[PathRef], extraEntries: Seq[String] = Nil)(implicit ctx: mill.api.Ctx.Dest) = {

  val thinClasspathEntries = replApiCp.map(_.path).flatMap{ cpRoot =>
    if (os.isFile(cpRoot) && cpRoot.ext == "jar") {
      val zip = new java.util.zip.ZipFile(cpRoot.toIO)
      import collection.JavaConverters._
      for(e <- zip.entries().asScala) yield e.getName
    }
    else if (os.isDir(cpRoot)) {
      for(sub <- os.walk(cpRoot)) yield sub.relativeTo(cpRoot).toString
    }
    else if (!os.exists(cpRoot)) Nil
    else throw new Exception(cpRoot.toString)
  }
  val allEntries = thinClasspathEntries ++ extraEntries
  os.write(
    ctx.dest / "ammonite-api-whitelist.txt",
    allEntries
      .flatMap(_.stripSuffix("/").split('/').inits)
      .filter(_.nonEmpty)
      .map(_.mkString("/"))
      .distinct
      .mkString("\n")
  )
  PathRef(ctx.dest)
}

object shell extends Cross[ShellModule](binCrossScalaVersions:_*)
class ShellModule(val crossScalaVersion: String) extends AmmModule{
  def moduleDeps = Seq(ops(), amm())
  object test extends Tests{
    def moduleDeps = super.moduleDeps ++ Seq(amm.repl().test)
    def thinWhitelist = T{
      generateApiWhitelist(
        amm.repl.api.full().exposedClassPath() ++
        Seq(amm.repl().test.compile().classes, compile().classes) ++
        resolveDeps(T.task{compileIvyDeps() ++ transitiveIvyDeps()})()
      )
    }

    def localClasspath = T{
      super.localClasspath() ++ Agg(thinWhitelist())
    }
    def forkEnv = super.forkEnv() ++ Seq(
      "AMMONITE_SHELL" -> shell().jar().path.toString,
      "AMMONITE_ASSEMBLY" -> amm().assembly().path.toString
    )
  }
}
object integration extends Cross[IntegrationModule](binCrossScalaVersions:_*)
class IntegrationModule(val crossScalaVersion: String) extends AmmInternalModule{
  def moduleDeps = Seq(ops(), amm())
  def ivyDeps = T{
    if (crossScalaVersion.startsWith("2.13."))
      Agg(ivy"com.lihaoyi::cask:0.6.0")
    else
      Agg.empty
  }
  object test extends Tests {
    def forkEnv = super.forkEnv() ++ Seq(
      "AMMONITE_SHELL" -> shell().jar().path.toString,
      "AMMONITE_ASSEMBLY" -> amm().launcher().path.toString
    )
  }
}

object sshd extends Cross[SshdModule](binCrossScalaVersions:_*)
class SshdModule(val crossScalaVersion: String) extends AmmModule{
  def moduleDeps = Seq(ops(), amm())
  def ivyDeps = Agg(
    // sshd-core 1.3.0 requires java8
    ivy"org.apache.sshd:sshd-core:1.2.0",
    ivy"org.bouncycastle:bcprov-jdk15on:1.56",
  )
  object test extends Tests {
    def ivyDeps = super.ivyDeps() ++ Agg(
      // slf4j-nop makes sshd server use logger that writes into the void
      ivy"org.slf4j:slf4j-nop:1.7.12",
      ivy"com.jcraft:jsch:0.1.54",
      ivy"org.scalacheck::scalacheck:1.14.0"
    )
  }
}

def unitTest(scalaVersion: String = sys.env("TRAVIS_SCALA_VERSION")) = T.command{
  ops(scalaVersion).test.test()()
  terminal(scalaVersion).test.test()()
  amm.repl(scalaVersion).test.test()()
  amm(scalaVersion).test.test()()
  shell(scalaVersion).test.test()()
  sshd(scalaVersion).test.test()()
}

def crossTest(scalaVersion: String = sys.env("TRAVIS_SCALA_VERSION")) = T.command{
  amm.repl.`cross-tests`(scalaVersion).test.test()()
}

def integrationTest(scalaVersion: String = sys.env("TRAVIS_SCALA_VERSION")) = T.command{
  integration(scalaVersion).test.test()()
}

def generateConstantsFile(version: String = buildVersion,
                          unstableVersion: String = "<fill-me-in-in-Constants.scala>",
                          bspVersion: String = "<fill-me-in-in-Constants.scala>",
                          curlUrl: String = "<fill-me-in-in-Constants.scala>",
                          unstableCurlUrl: String = "<fill-me-in-in-Constants.scala>",
                          oldCurlUrls: Seq[(String, String)] = Nil,
                          oldUnstableCurlUrls: Seq[(String, String)] = Nil,
                          scalaVersions: Seq[String] = fullCrossScalaVersions)
                         (implicit ctx: mill.util.Ctx.Dest)= {
  val scalaVersionsStr = scalaVersions.map("\"" + _ + "\"").mkString("Seq(", ", ", ")")
  val versionTxt = s"""
    package ammonite
    object Constants{
      val version = "$version"
      val unstableVersion = "$unstableVersion"
      val bspVersion = "$bspVersion"
      val curlUrl = "$curlUrl"
      val unstableCurlUrl = "$unstableCurlUrl"
      val oldCurlUrls = Seq[(String, String)](
        ${oldCurlUrls.map{case (name, value) => s""" "$name" -> "$value" """}.mkString(",\n")}
      )
      val oldUnstableCurlUrls = Seq[(String, String)](
        ${oldUnstableCurlUrls.map{case (name, value) => s""" "$name" -> "$value" """}.mkString(",\n")}
      )
      val scalaVersions = $scalaVersionsStr
    }
  """
  println("Writing Constants.scala")

  write(ctx.dest/"Constants.scala", versionTxt)
  ctx.dest/"Constants.scala"
}

def generateDependenciesFile(fileName: String,
                             deps: Seq[coursier.Dependency])
                            (implicit ctx: mill.util.Ctx.Dest) = {

  val dir = ctx.dest / "extra-resources"
  val dest = dir / fileName

  val content = deps
    .map { dep =>
      (dep.module.organization.value, dep.module.name.value, dep.version)
    }
    .sorted
    .map {
      case (org, name, ver) =>
        s"$org:$name:$ver"
    }
    .mkString("\n")

  mkdir(dir)
  println(s"Writing $dest")
  dir.toIO.mkdirs()
  write(dest, content.getBytes("UTF-8"))

  dir
}


def publishExecutable() = {
  if (!isMasterCommit) T.command{
    println("MISC COMMIT: generating executable but not publishing")
    mill.define.Task.sequence(latestAssemblies)()
  }else T.command{
    val latestAssemblyJars = mill.define.Task.sequence(latestAssemblies)()

    println("MASTER COMMIT: Creating a release")
    if (!unstable){
      requests.post(
        "https://api.github.com/repos/lihaoyi/Ammonite/releases",
        data = ujson.write(
          ujson.Obj(
            "tag_name" -> buildVersion,
            "name" -> buildVersion,
            "body" -> s"http://www.lihaoyi.com/Ammonite/#$buildVersion"
          )
        ),
        headers = Seq("Authorization" -> s"token ${sys.env("AMMONITE_BOT_AUTH_TOKEN")}")
      )
    }

    for ((version, jar) <- binCrossScalaVersions.zip(latestAssemblyJars)) {
      println("MASTER COMMIT: Publishing Executable for Scala " + version)
      //Prepare executable

      val scalaBinaryVersion = version.take(version.lastIndexOf("."))
      upload(
        jar.path,
        latestTaggedVersion,
        s"$scalaBinaryVersion-$buildVersion",
        sys.env("AMMONITE_BOT_AUTH_TOKEN")
      )
      upload(
        os.temp(
          os.read(os.pwd / "amm-template.sh")
            .replace("DEFAULT_AMM_VERSION=", s"DEFAULT_AMM_VERSION=$latestTaggedVersion")
        ),
        latestTaggedVersion,
        s"$scalaBinaryVersion-$buildVersion-bootstrap",
        sys.env("AMMONITE_BOT_AUTH_TOKEN")
      )
    }
  }
}

def publishDocs() = {
  val scalaVersion = binCrossScalaVersions.last
  // Disable doc auto-publishing for now, as the recent modularization means we
  // need to make significant changes to the readme and that'll time.
  if (!isMasterCommit) T.command{
    println("MISC COMMIT: Building readme for verification")
    %sbt(
      "readme/run",
      AMMONITE_SHELL=shell(scalaVersion).jar().path,
      AMMONITE_ASSEMBLY=amm(scalaVersion).assembly().path,
      CONSTANTS_FILE=generateConstantsFile()
    )
  }else T.command{
    println("MASTER COMMIT: Updating version and publishing to Github Pages")

    val publishDocs = sys.env("DEPLOY_KEY").replace("\\n", "\n")
    write(pwd / 'deploy_key, publishDocs)

    val (stableKey, unstableKey, oldStableKeys, oldUnstableKeys) =
      if (!unstable){
        (
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          for(v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
          for(v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion"
        )
      }else{
        (
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          s"$latestTaggedVersion/2.13-$buildVersion",
          for(v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
          for(v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$buildVersion"
        )
      }
    println("(stableKey, unstableKey)")
    println((stableKey, unstableKey))
    val constantsFile = generateConstantsFile(
      latestTaggedVersion,
      buildVersion,
      bspVersion,
      s"https://github.com/lihaoyi/Ammonite/releases/download/$stableKey",
      s"https://github.com/lihaoyi/Ammonite/releases/download/$unstableKey",
      for(k <- oldStableKeys)
        yield (k, s"https://github.com/lihaoyi/Ammonite/releases/download/$k"),
      for(k <- oldUnstableKeys)
        yield (k, s"https://github.com/lihaoyi/Ammonite/releases/download/$k")
    )

    %sbt(
      "readme/run",
      AMMONITE_SHELL=shell(scalaVersion).jar().path,
      AMMONITE_ASSEMBLY=amm(scalaVersion).assembly().path,
      CONSTANTS_FILE=constantsFile
    )
    %("ci/deploy_master_docs.sh")
  }
}

def partition(publishArtifacts: mill.main.Tasks[PublishModule.PublishData],
              shard: Int,
              divisionCount: Int) = {

  val groupedArtifacts = publishArtifacts.value
    .map{ t =>
      val taskCrossVersionOpt = t.ctx.segments.value
        .collectFirst{ case mill.define.Segment.Cross(List(v)) => v }
      val index = taskCrossVersionOpt
        .map(fullCrossScalaVersions.indexOf(_))
        .getOrElse(0)

      // Sort primarily on the scalaVersion, using the rendered name of the
      // task as the secondary sort key to break ties and ensure determinism
      t -> (index, t.ctx.segments.render)
    }
    .toMap

  val sortedArtifacts = publishArtifacts.value.sortBy(groupedArtifacts)

  val boundaries  =
    for(x <- 0 to divisionCount)
    yield math.round((x.toDouble * sortedArtifacts.length) / divisionCount).toInt

  sortedArtifacts.slice(boundaries(shard-1), boundaries(shard))

}

def publishSonatype(publishArtifacts: mill.main.Tasks[PublishModule.PublishData],
                    shard: Int,
                    divisionCount: Int) =
  T.command{

    val x: Seq[(Seq[(Path, String)], Artifact)] = {
      mill.define.Task.sequence(partition(publishArtifacts, shard, divisionCount))().map{
        case PublishModule.PublishData(a, s) => (s.map{case (p, f) => (p.path, f)}, a)
      }
    }
    if (isMasterCommit)
      new SonatypePublisher(
        "https://oss.sonatype.org/service/local",
        "https://oss.sonatype.org/content/repositories/snapshots",
        sys.env("SONATYPE_DEPLOY_USER") + ":" + sys.env("SONATYPE_DEPLOY_PASSWORD"),
        true,
        Seq("--passphrase", sys.env("SONATYPE_PGP_PASSWORD"), "--no-tty", "--pinentry-mode", "loopback", "--batch", "--yes", "-a", "-b"),
        120000,
        120000,
        T.ctx().log,
        120000,
      ).publishAll(
        true,
        x:_*
      )
  }


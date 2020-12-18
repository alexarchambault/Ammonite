package ammonite.interp.api

import coursierapi.{Dependency, Module}

object IvyConstructor extends IvyConstructor {

  def binaryVersion(version: String): String =
    if (version.forall(c => c.isDigit || c == '.'))
      version
        .stripPrefix("version ")
        .split('.')
        .take(2)
        .mkString(".")
    else
      version // milestones or RC are always fully cross-versioned

  val scalaBinaryVersion = binaryVersion(scala.util.Properties.versionString)

  val scalaFullBinaryVersion =
    scala.util.Properties
              .versionNumberString

}
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = Module.of(groupId, artifactId)
    def %%(artifactId: String) = Module.of(
      groupId,
      artifactId + "_" + IvyConstructor.scalaBinaryVersion
    )
  }
  implicit class ArtifactIdExt(t: Module){
    def %(version: String) = Dependency.of(t, version)
  }
}

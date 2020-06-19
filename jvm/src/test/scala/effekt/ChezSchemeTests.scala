package effekt

import java.io.File

import sbt.io._
import sbt.io.syntax._

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class ChezSchemeTests extends AnyFunSpec {

  // The sources of all testfiles are stored here:
  lazy val examplesDir = new File("examples")

  // Test files which are to be ignored (since features are missing or known bugs exist)
  lazy val ignored: List[File] = List(
    examplesDir / "pos" / "arrays.effekt",
    examplesDir / "pos" / "maps.effekt",
    examplesDir / "pos" / "dequeue.effekt",
    examplesDir / "pos" / "unsafe_cont.effekt",
    examplesDir / "pos" / "propagators.effekt"
  )

  runTestsIn(examplesDir)

  def runTestsIn(dir: File): Unit = describe(dir.getName) {
    dir.listFiles.foreach {
      case f if f.isDirectory => runTestsIn(f)
      case f if f.getName.endsWith(".effekt") || f.getName.endsWith(".md") =>
        val path = f.getParentFile
        val baseName = f.getName.stripSuffix(".md").stripSuffix(".effekt")

        val checkfile = path / (baseName + ".check")

        if (!checkfile.exists()) {
          sys error s"Missing checkfile for ${f.getPath}"
        }

        if (ignored.contains(f)) {
          ignore(f.getName) { () }
        } else {
          it(f.getName + " (call/cc)") {

            val out = interpretCS(f)
            if (checkfile.exists()) {
              assert(IO.read(checkfile).toString == out)
            }
          }
          it(f.getName + " (cps)") {

            val out = interpretCSLift(f)
            if (checkfile.exists()) {
              assert(IO.read(checkfile).toString == out)
            }
          }
        }

      case _ => ()
    }
  }

  def interpretCS(file: File): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--generator", "cs",
      "--includes", "chez/common",
      "--includes", ".",
      "--lib", "chez/lib"
    ))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "")
  }

  def interpretCSLift(file: File): String = {
    val compiler = new effekt.Driver {}
    val configs = compiler.createConfig(Seq(
      "--Koutput", "string",
      "--generator", "cslift",
      "--includes", "chez/common",
      "--includes", ".",
      "--lib", "chez/lift"
    ))
    configs.verify()
    compiler.compileFile(file.getPath, configs)
    configs.stringEmitter.result().replaceAll("\u001B\\[[;\\d]*m", "")
  }
}

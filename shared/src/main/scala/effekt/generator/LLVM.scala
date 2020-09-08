package effekt.generator

import effekt.context.Context
import effekt.core._
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol }

import org.bitbucket.inkytonik.kiama
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import effekt.context.assertions._

import scala.language.implicitConversions

import effekt.util.paths._

import scala.sys.process.Process

/**
 * It would be nice if Core could have an Effect Declaration or
 * translate effect declarations to Records...
 */
class LLVM extends Generator {

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath

  def llvmPath(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ll"

  def objectPath(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".o"

  /**
   * This is only called on the main entry point, we have to manually traverse the dependencies
   * and write them.
   */
  def run(src: Source)(implicit C: Context): Option[Document] = for {
    mod <- C.frontend(src)
    _ = C.checkMain(mod)
    deps = mod.dependencies.flatMap(dep => compile(dep))
    core <- C.lower(src)

    llvmFile = llvmPath(mod)
    result = LLVMPrinter.compilationUnit(mod, core, deps)
    _ = C.saveOutput(result.layout, llvmFile)

    objectFile = objectPath(mod)
    llcCommand = Process(Seq("llc-9", "-filetype=obj", "-o", objectFile, llvmFile))
    _ = C.config.output().emit(llcCommand.!!)

    mainFile = (C.config.libPath / "main.c").unixPath
    executableFile = path(mod)
    gccCommand = Process(Seq("gcc", mainFile, "-o", executableFile, objectFile))
    _ = C.config.output().emit(gccCommand.!!)

  } yield result

  /**
   * Compiles only the given module, does not compile dependencies
   */
  def compile(mod: Module)(implicit C: Context): Option[Document] = for {
    core <- C.lower(mod.source)
    doc = LLVMPrinter.format(core)
  } yield doc
}

object LLVMPrinter extends ParenPrettyPrinter {

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  val prelude = ""

  def compilationUnit(mod: Module, core: ModuleDecl, dependencies: List[Document])(implicit C: Context): Document =
    pretty {

      "define" <+> "i64" <+> "@effektMain" <> "()" <+> braces(
        "ret" <+> "i64" <+> "9"
      )

    }

  def moduleFile(path: String): String = path.replace('/', '_') + ".ll"

  def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(module(t))

  val emptyline: Doc = line <> line

  def module(m: ModuleDecl)(implicit C: Context): Doc = toDoc(m)

  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    emptyline

}

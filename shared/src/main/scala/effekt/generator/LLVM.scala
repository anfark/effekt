package effekt.generator

import effekt.context.Context
import effekt.machine._
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol, TermSymbol }

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
    mainName = C.checkMain(mod)

    mods = (mod.dependencies :+ mod).flatMap(m => C.evenLower(m.source))
    result = LLVMPrinter.compilationUnit(mainName, mods)

    llvmFile = llvmPath(mod)
    _ = C.saveOutput(result.layout, llvmFile)

    objectFile = objectPath(mod)
    llcCommand = Process(Seq("llc-9", "-filetype=obj", "-o", objectFile, llvmFile))
    _ = C.config.output().emit(llcCommand.!!)

    mainFile = (C.config.libPath / "main.c").unixPath
    executableFile = path(mod)
    gccCommand = Process(Seq("gcc", mainFile, "-o", executableFile, objectFile))
    _ = C.config.output().emit(gccCommand.!!)

  } yield result

}

object LLVMPrinter extends ParenPrettyPrinter {

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  val prelude = "define fastcc void @topLevel(%Sp %sp, i64 %res) { ret void }\n\n"

  def compilationUnit(mainName: TermSymbol, mods: List[ModuleDecl])(implicit C: Context): Document =
    pretty {

      "define" <+> "void" <+> "@effektMain" <> "()" <+> llvmBlock(
        "tail call fastcc void" <+> "@" <> nameDef(mainName) <> "()" <> line <>
          "ret void"
      )

    }

  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    emptyline

  // we prefix op$ to effect operations to avoid clashes with reserved names like `get` and `set`
  def nameDef(id: Symbol)(implicit C: Context): Doc =
    id.name.toString + "_" + id.id

  def llvmBlock(content: Doc): Doc = braces(nest(line <> content) <> line)

  val emptyline: Doc = line <> line

}

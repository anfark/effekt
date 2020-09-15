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

  def compilationUnit(mainName: TermSymbol, mods: List[ModuleDecl])(implicit C: Context): Document =
    pretty(

      vsep(mods.map(toDoc), line) <@@@>

        "define" <+> "void" <+> "@effektMain" <> "()" <+> llvmBlock(
          "%sp = tail call fastcc %Sp @initializeRts()" <@>
            // TODO generate and store topLevelCnt
            allocAndStoreSpp <@@@>
            jump(globalName(mainName), List())
        )

    )

  def toDoc(module: ModuleDecl)(implicit C: Context): Doc =
    module.decls.map(toDoc).foldRight(emptyDoc)(_ <@@@> _)

  def toDoc(decl: Decl)(implicit C: Context): Doc = decl match {
    case Def(functionName, evidence, parameters, body) =>
      // TODO parameters
      // TODO move out definition code
      "define fastcc void" <+> globalName(functionName) <+> "(%Sp %sp)" <+> llvmBlock(
        allocAndStoreSpp <@@@>
          toDoc(body)
      )
    case DefPrim(returnType, functionName, parameters, body) =>
      "define fastcc" <+> toDoc(returnType) <+> globalName(functionName) <>
        // TODO we can't use the unique id here, since we do not know it in the extern string.
        argumentList(parameters.map {
          case ValueParam(typ, id) => toDoc(typ) <+> "%" <> id.name.toString()
        }) <+>
        "alwaysinline" <+> llvmBlock(
          string(body)
        )
    case Include(content) =>
      string(content)
  }

  def toDoc(stmt: Stmt)(implicit C: Context): Doc = stmt match {
    case Let(name, expr, body) =>
      localName(name) <+> "=" <+> toDoc(expr) <@>
        toDoc(body)
    case Ret(valu) =>
      // TODO find type and generate loadCnt1 and Cnt1
      // TODO use fresh name for next
      "%next = call fastcc %Cnt1 @loadCnt1(%Sp* %spp)" <@>
        jump("%next", List(toDoc(valu)))
  }

  def toDoc(expr: Expr)(implicit C: Context): Doc = expr match {
    case AppPrim(returnType, blockName, args) =>
      "call" <+> toDoc(returnType) <+> globalName(blockName) <> argumentList(args.map(toDoc))
  }

  def toDoc(valu: Valu)(implicit C: Context): Doc = valu match {
    case IntLit(value)     => toDoc(PrimInt()) <+> value.toString()
    case BooleanLit(value) => toDoc(PrimBoolean()) <+> value.toString()
    case Var(typ, name)    => toDoc(typ) <+> localName(name)
  }

  def toDoc(param: Param)(implicit C: Context): Doc = param match {
    case ValueParam(typ, name) => toDoc(typ) <+> localName(name)
  }

  def toDoc(typ: Type)(implicit C: Context): Doc = typ match {
    case PrimUnit() =>
      // TODO choose different representation for unit
      "%Unit"
    case PrimInt() =>
      "%Int"
    case PrimBoolean() =>
      "%Boolean"
  }

  def jump(name: Doc, args: List[Doc]): Doc =
    // TODO use constant for spp name
    // TODO generate fresh name for newsp
    "%newsp = load %Sp, %Sp* %spp" <@>
      "tail call fastcc void" <+> name <> argumentList(("%Sp" <+> "%newsp") :: args) <@>
      "ret" <+> "void"

  val allocAndStoreSpp =
    "%spp = alloca %Sp" <@>
      "store %Sp %sp, %Sp* %spp"

  def localName(id: Symbol)(implicit C: Context): Doc =
    "%" <> nameDef(id)

  def globalName(id: Symbol)(implicit C: Context): Doc =
    "@" <> nameDef(id)

  def globalBuiltin(name: String)(implicit C: Context): Doc =
    "@" <> name

  // we prefix op$ to effect operations to avoid clashes with reserved names like `get` and `set`
  def nameDef(id: Symbol)(implicit C: Context): Doc =
    id.name.toString + "_" + id.id

  def llvmBlock(content: Doc): Doc = braces(nest(line <> content) <> line)

  implicit class MyDocOps(self: Doc) {
    def <@@@>(other: Doc): Doc = self <> emptyline <> other
  }

  def argumentList(args: List[Doc]) = parens(hsep(args, comma))

  val emptyline: Doc = line <> line

}

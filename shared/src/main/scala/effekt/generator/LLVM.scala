package effekt.generator

import effekt.context.Context
import effekt.machine._
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol, TermSymbol }

import org.bitbucket.inkytonik.kiama
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import org.bitbucket.inkytonik.kiama.util.Counter
import effekt.context.assertions._

import scala.language.implicitConversions

import effekt.util.paths._

import scala.sys.process.Process

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
    result = LLVMPrinter.compilationUnit(mainName, mods)(LLVMPrinter.LLVMContext(C))

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

  def compilationUnit(mainName: TermSymbol, mods: List[ModuleDecl])(implicit C: LLVMContext): Document =
    pretty(

      vsep(mods.map(toDoc), line) <@@@>

        "define" <+> "void" <+> "@effektMain" <> "()" <+> llvmBlock(
          "%sp = call fastcc %Sp @initializeRts()" <@>
            "%spp = alloca %Sp" <@>
            "store %Sp %sp, %Sp* %spp" <@@@>
            // TODO find return type of main
            storeCnt(PrimInt(), globalBuiltin("topLevel")) <@@@>
            jump(globalName(mainName), List())
        )

    )

  def toDoc(module: ModuleDecl)(implicit C: LLVMContext): Doc =
    onSeparateLines(module.decls.map(toDoc))

  def toDoc(decl: Decl)(implicit C: LLVMContext): Doc = decl match {
    case Def(functionName, evidence, params, body) => {
      val definitions = C.withDefinitions {
        C.emitDefinition {
          define(globalName(functionName), params.map(toDoc), toDoc(body))
        }
      }
      onSeparateLines(definitions.map(string))
    }
    case DefPrim(returnType, functionName, parameters, body) =>
      "define fastcc" <+> toDoc(returnType) <+> globalName(functionName) <>
        // we can't use unique id here, since we do not know it in the extern string.
        argumentList(parameters.map {
          case ValueParam(typ, id) => toDoc(typ) <+> "%" <> id.name.toString()
        }) <+>
        "alwaysinline" <+> llvmBlock(
          string(body)
        )
    case Include(content) =>
      string(content)
  }

  def toDoc(stmt: Stmt)(implicit C: LLVMContext): Doc = stmt match {
    case Let(name, expr, body) =>
      localName(name) <+> "=" <+> toDoc(expr) <@>
        toDoc(body)
    case Push(param, body, rest) =>
      val contName = globalBuiltin(freshName("k"))
      val vars = freeVars(body).filterNot(_.id == param.id)
      C.emitDefinition {
        define(contName, List(toDoc(param)),
          onLines(vars.map(load)) <@>
            toDoc(body))
      }
      onLines(vars.map(store)) <@>
        storeCnt(param.typ, contName) <@@@>
        toDoc(rest)
    case Ret(valu) =>
      val contName = "%" <> freshName("next")
      loadCnt(valueType(valu), contName) <@>
        jump(contName, List(toDoc(valu)))
    case Jump(name, args) =>
      jump(globalName(name), args.map(toDoc))
    case If(cond, thn, els) =>
      val thenName = freshName("then")
      val elseName = freshName("else")
      "br" <+> toDoc(cond) <>
        comma <+> "label" <+> "%" <> thenName <>
        comma <+> "label" <+> "%" <> elseName <@@@>
        thenName <> colon <@>
        toDoc(thn) <@@@>
        elseName <> colon <@>
        toDoc(els)
  }

  def toDoc(expr: Expr)(implicit C: LLVMContext): Doc = expr match {
    case AppPrim(returnType, blockName, args) =>
      "call fastcc" <+> toDoc(returnType) <+> globalName(blockName) <> argumentList(args.map(toDoc))
  }

  def toDoc(value: Value)(implicit C: LLVMContext): Doc = value match {
    case IntLit(n)      => toDoc(valueType(value)) <+> n.toString()
    case BooleanLit(b)  => toDoc(valueType(value)) <+> b.toString()
    case Var(typ, name) => toDoc(valueType(value)) <+> localName(name)
  }

  def toDoc(param: Param)(implicit C: LLVMContext): Doc = param match {
    case ValueParam(typ, name) => toDoc(typ) <+> localName(name)
  }

  def valueType(value: Value): Type = value match {
    case IntLit(_)     => PrimInt()
    case BooleanLit(_) => PrimBoolean()
    case Var(typ, _)   => typ
  }

  def toDoc(typ: Type)(implicit C: LLVMContext): Doc =
    "%" <> typeName(typ)

  def define(name: Doc, args: List[Doc], body: Doc): Doc =
    "define fastcc void" <+> name <> argumentList("%Sp noalias %sp" :: args) <+> llvmBlock(
      "%spp = alloca %Sp" <@>
        "store %Sp %sp, %Sp* %spp" <@@@>
        body
    )

  def jump(name: Doc, args: List[Doc])(implicit C: LLVMContext): Doc = {
    val newspName = "%" <> freshName("newsp")
    newspName <+> "=" <+> "load %Sp, %Sp* %spp" <@>
      "tail call fastcc void" <+> name <> argumentList(("%Sp" <+> newspName) :: args) <@>
      "ret" <+> "void"
  }

  def load(x: Var)(implicit C: LLVMContext): Doc =
    // TODO generate load_Typ on demand
    localName(x.id) <+> "=" <+> "call fastcc" <+> toDoc(x.typ) <+>
      globalBuiltin("load" + typeName(x.typ)) <> argumentList(List("%Sp* %spp"))

  def store(x: Var)(implicit C: LLVMContext): Doc =
    // TODO generate store_Typ on demand
    "call fastcc void" <+> globalBuiltin("store" + typeName(x.typ)) <>
      argumentList(List("%Sp* %spp", toDoc(x)))

  def loadCnt(typ: Type, contName: Doc): Doc =
    // TODO generate Cnt_Typ and loadCnt_Typ on demand
    contName <+> "=" <+> "call fastcc" <+> "%" <> cntTypeName(typ) <+>
      globalBuiltin("loadCnt" + typeName(typ)) <+> argumentList(List("%Sp* %spp"))

  def storeCnt(typ: Type, contName: Doc)(implicit C: LLVMContext): Doc =
    // TODO generate storeCnt_Typ on demand
    "call fastcc void" <+> globalBuiltin("storeCnt" + typeName(typ)) <>
      argumentList(List("%Sp* %spp", "%" <> cntTypeName(typ) <+> contName))

  def localName(id: Symbol): Doc =
    "%" <> nameDef(id)

  def globalName(id: Symbol): Doc =
    "@" <> nameDef(id)

  def nameDef(id: Symbol): Doc =
    id.name.toString + "_" + id.id

  def globalBuiltin(name: String): Doc =
    "@" <> name

  def typeName(typ: Type): String =
    typ match {
      case PrimInt()     => "Int"
      case PrimBoolean() => "Boolean"
      case PrimUnit()    => "Unit"
    }

  def cntTypeName(typ: Type): String =
    "Cnt" + typeName(typ)

  def freshName(name: String)(implicit C: LLVMContext): String =
    name + "_" + C.fresh.next().toString()

  def llvmBlock(content: Doc): Doc = braces(nest(line <> content) <> line)

  implicit class MyDocOps(self: Doc) {
    def <@@@>(other: Doc): Doc = self <> emptyline <> other
  }

  def argumentList(args: List[Doc]) = parens(hsep(args, comma))

  def onLines(docs: List[Doc]): Doc =
    docs.foldRight(emptyDoc)(_ <@> _)

  def onSeparateLines(docs: List[Doc]): Doc =
    docs.foldRight(emptyDoc)(_ <@@@> _)

  val emptyline: Doc = line <> line

  /**
   * Find free variables
   */

  def freeVars(stmt: Stmt): List[Var] = stmt match {
    case Let(name, expr, rest) =>
      freeVars(expr) ++ freeVars(rest).filterNot(_.id == name)
    case Push(param, body, rest) =>
      freeVars(body).filterNot(_.id == param.id) ++ freeVars(rest)
    case Ret(value) =>
      freeVars(value)
    case Jump(_, args) =>
      args.map(freeVars).flatten
    case If(cond, thn, els) =>
      freeVars(cond) ++ freeVars(thn) ++ freeVars(els)
  }

  def freeVars(expr: Expr): List[Var] = expr match {
    case AppPrim(_, _, args) => args.map(freeVars).flatten
  }

  def freeVars(value: Value): List[Var] = value match {
    case v: Var        => List(v)
    case i: IntLit     => List()
    case b: BooleanLit => List()
  }

  /**
   * Extra info in context
   */

  case class LLVMContext(context: Context) {
    val fresh = new Counter(0)

    private var definitions: List[String] = List()

    def emitDefinition(d: Doc): Unit =
      this.definitions = pretty(d).layout :: this.definitions

    def withDefinitions[R](block: => R): List[String] = {
      this.definitions = List();
      val result = block;
      this.definitions
    }

  }

  private implicit def asContext(C: LLVMContext): Context = C.context
  private implicit def getContext(implicit C: LLVMContext): Context = C.context

}

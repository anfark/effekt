package effekt.generator

import effekt.context.Context
import effekt.machine._
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol, BlockSymbol, ValueSymbol }

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

  def compilationUnit(mainName: BlockSymbol, mods: List[ModuleDecl])(implicit C: LLVMContext): Document =
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
    case Def(functionName, BlockLit(params, body)) => {
      val definitions = C.withGlobalDefs {
        C.emitGlobalDef {
          define(globalName(functionName), params.map(toDoc),
            C.withLocalDefs(body) {
              toDoc(body) <@@@>
                onSeparateLines(localDefsBasicBlocks())
            })
        }
      };
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
      // TODO account for local defs
      C.emitGlobalDef {
        define(contName, List(toDoc(param)),
          onLines(vars.map(load)) <@>
            toDoc(body))
      }
      onLines(vars.map(store)) <@>
        storeCnt(param.typ, contName) <@@@>
        toDoc(rest)
    case DefLocal(name, block, rest) =>
      toDoc(rest)
    case Ret(valu) =>
      val contName = "%" <> freshName("next")
      loadCnt(valueType(valu), contName) <@>
        jump(contName, List(toDocWithType(valu)))
    case Jump(name, args) =>
      if (C.getLocalDefs.isDefinedAt(name)) {
        "br" <+> "label" <+> localName(name)
      } else {
        jump(globalName(name), args.map(toDocWithType))
      }
    case If(cond, thenBlock, elseBlock) =>
      "br" <+> toDocWithType(cond) <> comma <+>
        "label" <+> localName(thenBlock) <+> comma <+> "label" <+> localName(elseBlock)
  }

  def toDoc(expr: Expr)(implicit C: LLVMContext): Doc = expr match {
    case AppPrim(returnType, blockName, args) =>
      "call fastcc" <+> toDoc(returnType) <+> globalName(blockName) <> argumentList(args.map(toDocWithType))
  }

  // TODO find better place for this...
  def localDefsBasicBlocks()(implicit C: LLVMContext): List[Doc] = {
    C.getLocalDefs.toList.map {
      case (blockName, BlockLit(params, body)) =>
        nameDef(blockName) <> colon <@>
          onLines(phiInstructions(C.getLocalDefs, blockName, params).map(toDoc)) <@>
          toDoc(body)
    }
  }

  def toDocWithType(value: Value)(implicit C: LLVMContext): Doc =
    toDoc(valueType(value)) <+> toDoc(value)

  def toDoc(value: Value)(implicit C: LLVMContext): Doc = value match {
    case IntLit(n)      => n.toString()
    case BooleanLit(b)  => b.toString()
    case Var(typ, name) => localName(name)
  }

  def toDoc(param: ValueParam)(implicit C: LLVMContext): Doc = param match {
    case ValueParam(typ, name) => toDoc(typ) <+> localName(name)
  }

  def valueType(value: Value): Type = value match {
    case IntLit(_)     => PrimInt()
    case BooleanLit(_) => PrimBoolean()
    case Var(typ, _)   => typ
  }

  def toDoc(typ: Type)(implicit C: LLVMContext): Doc =
    "%" <> typeName(typ)

  def toDoc(phi: Phi)(implicit C: LLVMContext): Doc =
    phi match {
      case Phi(ValueParam(typ, name), args) => {
        localName(name) <+> "=" <+> "phi" <+> toDoc(typ) <+>
          hsep(args.toList.map {
            case (label, value) =>
              brackets(toDoc(value) <> comma <+> localName(label))
          }, comma)
      }
    }

  /**
   * Auxiliary macros
   */

  def define(name: Doc, args: List[Doc], body: Doc): Doc =
    "define fastcc void" <+> name <> argumentList("%Sp noalias %sp" :: args) <+>
      llvmBlock(
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
      argumentList(List("%Sp* %spp", toDocWithType(x)))

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
    case DefLocal(name, block, rest) =>
      freeVars(block) ++ freeVars(rest)
    case Ret(value) =>
      freeVars(value)
    case Jump(_, args) =>
      args.map(freeVars).flatten
    case If(cond, thn, els) =>
      freeVars(cond)
  }
  def freeVars(expr: Expr): List[Var] = expr match {
    case AppPrim(_, _, args) => args.map(freeVars).flatten
  }

  def freeVars(block: BlockLit): List[Var] = block match {
    case BlockLit(params, body) =>
      freeVars(body).filterNot(v => params.exists(param => v.id == param.id))
  }

  def freeVars(value: Value): List[Var] = value match {
    case v: Var        => List(v)
    case i: IntLit     => List()
    case b: BooleanLit => List()
  }

  /**
   * Gather local definitions
   */

  def gatherLocalDefs(stmt: Stmt): Map[BlockSymbol, BlockLit] = stmt match {
    case Let(_, _, rest) =>
      gatherLocalDefs(rest)
    case Push(_, body, rest) =>
      gatherLocalDefs(body) ++ gatherLocalDefs(rest)
    case DefLocal(name, block, rest) =>
      Map(name -> block) ++ gatherLocalDefs(block.body) ++ gatherLocalDefs(rest)
    case Ret(_) =>
      Map()
    case Jump(_, _) =>
      Map()
    case If(_, _, _) =>
      Map()
  }

  case class Phi(param: ValueParam, args: List[(BlockSymbol, Value)])

  def phiInstructions(localDefs: Map[BlockSymbol, BlockLit], blockName: BlockSymbol, params: List[ValueParam]): List[Phi] = {
    val predecessors = localDefs.flatMap {
      case (predecessorName, BlockLit(_, body)) =>
        findJumpArgsTo(blockName, body).map {
          args => (predecessorName -> args)
        }
    };
    val transposedPredecessors = predecessors.toList.map {
      case (predecessorName, args) => args.map {
        arg => (predecessorName, arg)
      }
    }.transpose;
    params.zip(transposedPredecessors).map {
      case (param, argsFromBlocks) => Phi(param, argsFromBlocks)
    }
  }

  def findJumpArgsTo(blockName: Symbol, stmt: Stmt): Option[List[Value]] =
    stmt match {
      case Let(_, _, rest) =>
        findJumpArgsTo(blockName, rest)
      case Push(_, _, rest) =>
        findJumpArgsTo(blockName, rest)
      case DefLocal(_, _, rest) =>
        findJumpArgsTo(blockName, rest)
      case Ret(_) =>
        None
      case Jump(name, args) =>
        if (name == blockName) {
          Some(args)
        } else {
          None
        }
      case If(_, _, _) =>
        None
    }

  /**
   * Extra info in context
   */

  case class LLVMContext(context: Context) {
    val fresh = new Counter(0)

    private var globalDefs: List[String] = List()

    def emitGlobalDef(d: Doc): Unit =
      this.globalDefs = pretty(d).layout :: this.globalDefs

    def withGlobalDefs[R](prog: => R): List[String] = {
      this.globalDefs = List();
      val result = prog;
      this.globalDefs
    }

    private var localDefsMap: Map[BlockSymbol, BlockLit] = Map()

    def withLocalDefs[R](stmt: Stmt)(prog: => R): R = {
      this.localDefsMap = gatherLocalDefs(stmt)
      prog
    }

    def getLocalDefs: Map[BlockSymbol, BlockLit] =
      this.localDefsMap

  }

  private implicit def asContext(C: LLVMContext): Context = C.context
  private implicit def getContext(implicit C: LLVMContext): Context = C.context

}

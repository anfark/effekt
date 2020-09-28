package effekt.generator

import effekt.context.Context
import effekt.machine._
import effekt.machine.FreshValueSymbol
import effekt.symbols.Module
import effekt.symbols.{ BlockSymbol, Name, Symbol, ValueSymbol }
import org.bitbucket.inkytonik.kiama
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import org.bitbucket.inkytonik.kiama.util.Counter
import effekt.context.assertions._
import effekt.util.GenericPrinter

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

      val (localDefs, entry) = blockFloat(parameterLift(body));

      val globalDefinitions = onSeparateLines {
        pushedLocalDefs(body).map { blockName =>

          val blockParams = localDefs(blockName).params;
          val freshVarNames: List[Var] = blockParams.map { param =>
            Var(param.typ, FreshValueSymbol(param.id.name.name, C.module))
          };
          val freshParamNames = freshVarNames.take(1).map {
            v => ValueParam(v.typ, v.id)
          };

          val entryBlockName: BlockSymbol = FreshBlockSymbol("entry", C.module);
          val entryBlock = BlockLit(List(), JumpLocal(blockName, freshVarNames));
          val basicBlocks = reachableBasicBlocks(
            entryBlockName,
            localDefs + (entryBlockName -> entryBlock)
          );
          val phiInstructionsMap = findPhiInstructions(basicBlocks);

          define(globalName(blockName), freshParamNames.map(toDoc), {

            toDoc(JumpLocal(entryBlockName, List())) <@@@>
              onSeparateLines {
                basicBlocks.map {
                  case (blockName, BlockLit(_, blockBody)) =>
                    toDocBasicBlock(
                      blockName, blockBody, phiInstructionsMap,
                      Map(entryBlockName -> freshVarNames.drop(1))
                    );
                }
              }
          })
        }
      };

      val thisDefinition = define(globalName(functionName), params.map(toDoc), {

        val entryBlockName: BlockSymbol = FreshBlockSymbol("entry", C.module);
        val entryBlock = BlockLit(List(), entry);
        val basicBlocks = reachableBasicBlocks(
          entryBlockName,
          localDefs + (entryBlockName -> entryBlock)
        );
        val phiInstructionsMap = findPhiInstructions(basicBlocks);

        toDoc(JumpLocal(entryBlockName, List())) <@@@>
          onSeparateLines {
            basicBlocks.map {
              case (blockName, BlockLit(_, blockBody)) =>
                toDocBasicBlock(
                  blockName, blockBody, phiInstructionsMap, Map()
                )
            }
          }
      })

      globalDefinitions <@@@> thisDefinition

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
    case DefLocal(name, block, rest) =>
      toDoc(rest)
    case Push(typ, blockName, freeVars, rest) => {
      onLines(freeVars.map(store)) <@>
        storeCnt(typ, globalName(blockName)) <@@@>
        toDoc(rest)
    }
    case Ret(valu) =>
      val contName = "%" <> freshName("next")
      loadCnt(valueType(valu), contName) <@>
        jump(contName, List(toDocWithType(valu)))
    case Jump(name, args) =>
      jump(globalName(name), args.map(toDocWithType))
    case JumpLocal(name, args) =>
      "br" <+> "label" <+> localName(name)
    case If(cond, thenBlock, _, elseBlock, _) =>
      "br" <+> toDocWithType(cond) <> comma <+>
        "label" <+> localName(thenBlock) <+> comma <+> "label" <+> localName(elseBlock)
  }

  def toDoc(expr: Expr)(implicit C: LLVMContext): Doc = expr match {
    case AppPrim(returnType, blockName, args) =>
      "call fastcc" <+> toDoc(returnType) <+> globalName(blockName) <> argumentList(args.map(toDocWithType))
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

  def toDocBasicBlock(blockName: BlockSymbol, blockBody: Stmt, phiInstructionsMap: Map[BlockSymbol, List[Phi]], loadInstructionsMap: Map[BlockSymbol, List[Var]])(implicit C: LLVMContext): Doc = {
    // TODO merge phi instructions and load instructions into same map
    nameDef(blockName) <> colon <@>
      onLines(phiInstructionsMap.getOrElse(blockName, List()).map(toDoc)) <@>
      onLines(loadInstructionsMap.getOrElse(blockName, List()).reverse.map(load)) <@>
      toDoc(blockBody)

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

  def store(v: Value)(implicit C: LLVMContext): Doc =
    // TODO generate store_Typ on demand
    "call fastcc void" <+> globalBuiltin("store" + typeName((valueType(v)))) <>
      argumentList(List("%Sp* %spp", toDocWithType(v)))

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

  def onLines(docs: Iterable[Doc]): Doc =
    docs.foldRight(emptyDoc)(_ <@> _)

  def onSeparateLines(docs: Iterable[Doc]): Doc =
    docs.foldRight(emptyDoc)(_ <@@@> _)

  val emptyline: Doc = line <> line

  /**
   * Analysis on terms
   */

  def freeVars(stmt: Stmt): Set[Var] = stmt match {
    case Let(name, expr, rest) =>
      freeVars(expr) ++ freeVars(rest).filterNot(_.id == name)
    case DefLocal(name, block, rest) =>
      freeVars(block) ++ freeVars(rest)
    case Push(_, _, args, rest) =>
      args.flatMap(freeVars).toSet ++ freeVars(rest)
    case Ret(value) =>
      freeVars(value)
    case Jump(_, args) =>
      args.flatMap(freeVars).toSet
    case JumpLocal(_, args) =>
      args.flatMap(freeVars).toSet
    case If(cond, thenBlock, thenArgs, elseBlock, elseArgs) =>
      freeVars(cond) ++ thenArgs.flatMap(freeVars) ++ elseArgs.flatMap(freeVars)
  }
  def freeVars(expr: Expr): Set[Var] = expr match {
    case AppPrim(_, _, args) => args.flatMap(freeVars).toSet
  }

  def freeVars(block: BlockLit): Set[Var] = block match {
    case BlockLit(params, body) =>
      freeVars(body).filterNot(v => params.exists(param => v.id == param.id))
  }

  def freeVars(value: Value): Set[Var] = value match {
    case v: Var        => Set(v)
    case i: IntLit     => Set()
    case b: BooleanLit => Set()
  }

  def substitute(mapping: Map[ValueSymbol, Value], stmt: Stmt): Stmt = stmt match {
    case Let(x, expr, rest) =>
      Let(x, substitute(mapping, expr), substitute(mapping, rest))
    case DefLocal(blockName, BlockLit(params, body), rest) =>
      DefLocal(blockName, BlockLit(params, substitute(mapping, body)),
        substitute(mapping, rest))
    case Push(typ, blockName, blockArgs, rest) =>
      Push(typ, blockName, blockArgs.map(substitute(mapping, _)), substitute(mapping, rest))
    case Ret(expr) =>
      Ret(substitute(mapping, expr))
    case Jump(blockName, blockArgs) =>
      Jump(blockName, blockArgs.map(substitute(mapping, _)))
    case JumpLocal(blockName, blockArgs) =>
      JumpLocal(blockName, blockArgs.map(substitute(mapping, _)))
    case If(cond, thenBlockName, thenArgs, elseBlockName, elseArgs) =>
      If(
        cond,
        thenBlockName, thenArgs.map(substitute(mapping, _)),
        elseBlockName, elseArgs.map(substitute(mapping, _))
      )
  }

  def substitute(mapping: Map[ValueSymbol, Value], expr: Expr): Expr = expr match {
    case AppPrim(typ, blockName, args) =>
      AppPrim(typ, blockName, args.map(substitute(mapping, _)))
  }

  def substitute(mapping: Map[ValueSymbol, Value], value: Value): Value = value match {
    case IntLit(n)      => IntLit(n)
    case BooleanLit(b)  => BooleanLit(b)
    case Var(typ, name) => mapping.getOrElse(name, Var(typ, name))
  }

  def parameterLift(stmt: Stmt)(implicit C: LLVMContext): Stmt = stmt match {
    case Let(x, e, rest) =>
      Let(x, e, parameterLift(rest))
    case DefLocal(name, BlockLit(params, body), rest) =>
      val vars = freeVars(BlockLit(params, body)).toList
      val freshVars = vars.map(v =>
        Var(v.typ, FreshValueSymbol(v.id.name.name, C.module)));
      val freshParams = freshVars.map { v => ValueParam(v.typ, v.id) }
      val mapping = vars.map(_.id).zip(freshVars).toMap;
      DefLocal(name, BlockLit(
        params ++ freshParams,
        parameterLift(substitute(mapping, addArguments(name, vars, body)))
      ),
        parameterLift(addArguments(name, vars, rest)))
    case Push(typ, blockName, blockArgs, rest) =>
      Push(typ, blockName, blockArgs, parameterLift(rest))
    case Ret(expr) =>
      Ret(expr)
    case Jump(blockName, args) =>
      Jump(blockName, args)
    case JumpLocal(blockName, args) =>
      JumpLocal(blockName, args)
    case If(cond, thenBlockName, thenArgs, elseBlockName, elseArgs) =>
      If(cond, thenBlockName, thenArgs, elseBlockName, elseArgs)
  }

  def addArguments(name: BlockSymbol, args: List[Var], stmt: Stmt): Stmt =
    stmt match {
      case Let(x, e, rest) =>
        Let(x, e, addArguments(name, args, rest))
      case DefLocal(blockName, BlockLit(params, body), rest) =>
        DefLocal(blockName, BlockLit(
          params,
          addArguments(name, args, body)
        ),
          addArguments(name, args, rest))
      case Push(typ, blockName, blockArgs, rest) =>
        val newArgs = if (blockName == name) { args } else { List() };
        Push(typ, blockName, blockArgs ++ newArgs, addArguments(name, args, rest))
      case Ret(expr) =>
        Ret(expr)
      case Jump(blockName, blockArgs) =>
        val newArgs = if (blockName == name) { args } else { List() };
        Jump(blockName, blockArgs ++ newArgs)
      case JumpLocal(blockName, blockArgs) =>
        val newArgs = if (blockName == name) { args } else { List() };
        JumpLocal(blockName, blockArgs ++ newArgs)
      case If(cond, thenBlockName, thenArgs, elseBlockName, elseArgs) =>
        val newThenArgs = if (thenBlockName == name) { args } else { List() };
        val newElseArgs = if (elseBlockName == name) { args } else { List() };
        If(
          cond,
          thenBlockName, thenArgs ++ newThenArgs,
          elseBlockName, elseArgs ++ newElseArgs
        )
    }

  def blockFloat(stmt: Stmt): (Map[BlockSymbol, BlockLit], Stmt) = stmt match {
    case Let(x, e, rest) =>
      val (defs, nakedRest) = blockFloat(rest);
      (defs, Let(x, e, nakedRest))
    case DefLocal(name, BlockLit(params, body), rest) =>
      val (blockDefs, nakedBody) = blockFloat(body);
      val (restDefs, nakedRest) = blockFloat(rest);
      val defs = Map(name -> BlockLit(params, nakedBody)) ++ blockDefs ++ restDefs;
      (defs, nakedRest)
    case Push(typ, blockName, blockArgs, rest) =>
      val (defs, nakedRest) = blockFloat(rest);
      (defs, (Push(typ, blockName, blockArgs, nakedRest)))
    case Ret(expr) =>
      (Map(), Ret(expr))
    case Jump(blockName, args) =>
      (Map(), Jump(blockName, args))
    case JumpLocal(blockName, args) =>
      (Map(), JumpLocal(blockName, args))
    case If(cond, thenBlockName, thenArgs, elseBlockName, elseArgs) =>
      (Map(), If(cond, thenBlockName, thenArgs, elseBlockName, elseArgs))
  }

  case class Phi(param: ValueParam, args: List[(BlockSymbol, Value)])

  def findPhiInstructions(basicBlocks: Map[BlockSymbol, BlockLit]): Map[BlockSymbol, List[Phi]] = {

    val predecessorsMap = gather(basicBlocks.toList.flatMap {
      case (callerName, BlockLit(_, blockBody)) =>
        localJumpTargets(blockBody).map {
          case (calleeName, args) =>
            val params = basicBlocks(calleeName).params;
            (calleeName, (callerName, params.zip(args).toMap))
        }
    });

    val phiInstructionsMap: Map[BlockSymbol, List[Phi]] =
      predecessorsMap.view.mapValues {
        case calledFromMap =>
          transpose(calledFromMap).map {
            case (param, args) => Phi(param, args.toList)
          }.toList
      }.toMap;

    phiInstructionsMap
  }

  def gather[K, V](entries: Iterable[(K, V)]): Map[K, Iterable[V]] = {
    entries.groupMap(x => x._1)(x => x._2)
  }

  def transpose[K1, K2, V](nested: Iterable[(K1, Iterable[(K2, V)])]): Map[K2, Iterable[(K1, V)]] = {
    val triples = nested.flatMap {
      case (k1, inner) => inner.map {
        case (k2, v) => (k1, k2, v)
      }
    }
    triples.groupMap(x => x._2)(x => (x._1, x._3))
  }

  def localJumpTargets(stmt: Stmt): Map[BlockSymbol, List[Value]] =
    stmt match {
      case Let(_, _, rest) =>
        localJumpTargets(rest)
      case Push(_, _, _, rest) =>
        localJumpTargets(rest)
      case DefLocal(_, _, rest) =>
        localJumpTargets(rest)
      case Ret(_) =>
        Map()
      case Jump(targetName, args) =>
        Map()
      case JumpLocal(targetName, args) =>
        Map(targetName -> args)
      case If(_, thenName, thenArgs, elseName, elseArgs) =>
        // TODO what if both branches goto the same block?
        Map(thenName -> thenArgs) ++ Map(elseName -> elseArgs)
    }

  def reachableBasicBlocks(entryBlockName: BlockSymbol, basicBlocks: Map[BlockSymbol, BlockLit]): Map[BlockSymbol, BlockLit] = {

    val reachableBlocksSet: Set[BlockSymbol] = basicBlocks.flatMap {
      case (_, BlockLit(_, body)) => localJumpTargets(body).keys
    }.toSet + entryBlockName

    val reachableBasicBlocks = basicBlocks.view.filterKeys { blockName =>
      reachableBlocksSet.contains(blockName)
    }.toMap

    reachableBasicBlocks
  }

  def pushedLocalDefs(stmt: Stmt): Set[BlockSymbol] =
    stmt match {
      case Let(_, _, rest) =>
        pushedLocalDefs(rest)
      case Push(_, name, args, rest) =>
        Set(name) ++ pushedLocalDefs(rest)
      case DefLocal(_, BlockLit(_, body), rest) =>
        pushedLocalDefs(body) ++ pushedLocalDefs(rest)
      case Ret(_) =>
        Set()
      case Jump(name, args) =>
        Set()
      case JumpLocal(name, args) =>
        Set()
      case If(_, _, _, _, _) =>
        Set()
    }

  /**
   * Extra info in context
   */

  case class LLVMContext(context: Context) {
    val fresh = new Counter(0)
  }

  private implicit def asContext(C: LLVMContext): Context = C.context
  private implicit def getContext(implicit C: LLVMContext): Context = C.context

}

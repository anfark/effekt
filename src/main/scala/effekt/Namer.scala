package effekt
package namer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import context.{ CompilerContext, Phase }
import effekt.source.{ Def, Id, Tree }
import effekt.source.traversal._
import effekt.symbols._
import effekt.util.scopes._

import org.bitbucket.inkytonik.kiama.util.Source

/**
 * The output of this phase: a mapping from source identifier to symbol
 *
 * It contains both, TermSymbols and TypeSymbols
 *
 * There is an important distinction between:
 *   - resolving: that is looking up symbols (might include storing the result into the symbolTable)
 *   - binding: that is adding a binding to the environment (lexical.Scope)
 */
case class NamerState(
  source: Source,
  module: effekt.source.ModuleDecl,
  terms: Scope[TermSymbol],
  types: Scope[TypeSymbol]
)

class Namer extends Phase { namer =>

  val name = "Namer"

  def run(src: Source, decl: source.ModuleDecl, compiler: CompilerContext): Module = {

    val topLevelTerms = toplevel[TermSymbol](builtins.rootTerms)
    val topLevelTypes = toplevel[TypeSymbol](builtins.rootTypes)

    val (terms, types) = decl.imports.foldLeft((topLevelTerms, topLevelTypes)) {
      case ((terms, types), source.Import(path)) =>
        val mod = compiler.resolve(path)
        (terms.enterWith(mod.terms), types.enterWith(mod.types))
    }

    val state = NamerState(src, decl, terms.enter, types.enter)
    compiler.namerState = state
    compiler.phases.init(this)


    resolve(given compiler)(decl)

    Module(
      decl,
      src,
      state.terms.bindings.toMap,
      state.types.bindings.toMap)
  }

  /**
   * An effectful traversal with two side effects:
   * 1) the passed environment is enriched with definitions
   * 2) names are resolved using the environment and written to the table
   */
  val resolve: Traversal[Tree, CompilerContext] = Compiler.focusing {

    // (1) === Binding Occurrences ===
    case source.ModuleDecl(path, imports, decls) =>
      decls foreach { resolveDef(true) }
      Compiler scoped { resolveAll(decls) }

    case source.DefStmt(d, rest) =>
      Compiler scoped {
        resolveDef(false)(d)
        resolve(d)
        resolve(rest)
      }

    case source.ValueParam(id, tpe) =>
      id := ValueParam(id.localName, tpe.map(resolveValueType))

    case source.BlockParam(id, tpe) =>
      id := BlockParam(id.localName, resolveBlockType(tpe))

    // FunDef and EffDef have already been resolved as part of the module declaration
    case f @ source.FunDef(id, tparams, params, ret, body) =>
      val funSym = Compiler.get(f)
      Compiler scoped {
        funSym.tparams.foreach { p =>
          Compiler.bind(p)
        }
        Compiler.bind(funSym.params)
        resolve(body)
      }

    case source.EffDef(id, tparams, params, ret) => ()
    case source.DataDef(id, tparams, ctors) => ()
    case source.ExternType(id, tparams) => ()
    case source.ExternEffect(id, tparams) => ()
    case source.ExternFun(pure, id, tparams, params, ret, body) => ()
    case source.ExternInclude(path) => ()

    case source.TryHandle(body, clauses) =>
      Compiler scoped { resolve(body) }
      resolveAll(clauses)

    case source.OpClause(op, params, body, resumeId) =>
      Compiler.at(op) { op.resolveTerms() }
      val ps = params.map(resolveValueParams)
      Compiler scoped {
        Compiler.bind(ps)
        resumeId := ResumeParam()
        resolve(body)
      }

    case source.Clause(op, params, body) =>
      Compiler.at(op) { op.resolveTerms() }
      val ps = params.map(resolveValueParams)
      Compiler scoped {
        Compiler.bind(ps)
        resolve(body)
      }

    case source.BlockArg(params, stmt) =>
      val ps = resolveValueParams(source.ValueParams(params)) // TODO drop wrapping after refactoring
      Compiler scoped {
        Compiler.bind(List(ps))
        resolve(stmt)
      }

    // (2) === Bound Occurrences ===

    case source.Call(id, targs, args) =>
      id.resolveTerms {
        case b : (BlockParam | ResumeParam | Fun) => b
        case _ => Compiler.error("Expected callable")
      }
      targs foreach resolveValueType
      resolveAll(args)

    case source.Var(id) => id.resolveTerms {
      case b : BlockParam => Compiler.error("Blocks have to be fully applied and can't be used as values.")
      case other => other
    }

    case tpe: source.ValueType => resolveValueType(tpe)
    case tpe: source.BlockType => resolveBlockType(tpe)

    // THIS COULD ALSO BE A TYPE!
    case id : Id => id.resolveTerms()

    case other => resolveAll(other)
  }

  val resolveAll: Traversal[Any, CompilerContext] = all(resolve)

  /**
   * Resolve Parameters as part of resolving function signatures
   *
   * Since we annotate functions and effect declarations with resolved types, we need to
   * resolve the parameters.
   *
   * Importantly, resolving them will *not* add the parameters as binding occurence in the current scope.
   * This is done separately by means of `bind`
   */
  def resolveParamSection(params: source.ParamSection)(given CompilerContext): List[Param] = params match {
    case ps : source.ValueParams => resolveValueParams(ps)
    case source.BlockParam(id, tpe) =>
      val sym = BlockParam(id.localName, resolveBlockType(tpe))
      Compiler.put(id, sym)
      List(sym)
  }
  def resolveValueParams(ps: source.ValueParams)(given CompilerContext): List[ValueParam] =
    ps.params map { p =>
      val sym = ValueParam(p.id.localName, p.tpe.map(resolveValueType))
      Compiler.put(p.id, sym)
      sym
    }

  // TODO consider setting owner, instead of this qualify hack
  def resolveDef(qualify: Boolean): Traversal[Def, CompilerContext] = {

    def name(id: Id) = if (qualify) {
      id.qualifiedName
    } else {
      id.localName
    }

    Compiler.focusing {

      case d @ source.ValDef(id, annot, binding) =>
        val tpe = annot.map(resolveValueType)
        resolve(binding)
        id := ValBinder(id.localName, tpe, d)

      case d @ source.VarDef(id, annot, binding) =>
        val tpe = annot.map(resolveValueType)
        resolve(binding)
        id := VarBinder(id.localName, tpe, d)

      case f @ source.FunDef(id, tparams, params, annot, body) =>
        val sym = Compiler scoped {
          // we create a new scope, since resolving type params introduces them in this scope
          UserFunction(id.freshTermName(qualify), tparams map resolveTypeParam, params map resolveParamSection, annot map resolveEffectful, f)
        }
        id := sym

      case e @ source.EffDef(id, tparams, params, ret) =>
        // we use the localName for effects, since they will be bound as capabilities
        val effectSym = UserEffect(id.localName, Nil)
        val opSym = Compiler scoped {
          val tps = tparams map resolveTypeParam
          val tpe = Effectful(resolveValueType(ret), Effects(List(effectSym)))
          EffectOp(id.localName, tps, params map resolveValueParams, Some(tpe), effectSym)
        }
        effectSym.ops = List(opSym)
        // we would need a second id that is the definition of the operation
        id := effectSym
        Compiler.bind(opSym)

      case d @ source.DataDef(id, tparams, ctors) =>
        val (typ, tps) = Compiler scoped {
          val tps = tparams map resolveTypeParam
          (DataType(name(id), tps), tps)
        }
        id := typ
        val cs = ctors map {
          case source.Constructor(id, ps) =>
            val sym = Compiler scoped {
              tps.foreach { t => Compiler.bind(t) }
              Constructor(name(id), ps map resolveValueParams, typ)
            }
            id := sym
            sym
        }
        typ.ctors = cs

      case d @ source.ExternType(id, tparams) =>
        id := Compiler scoped {
          val tps = tparams map resolveTypeParam
          BuiltinType(name(id), tps)
        }

      case d @ source.ExternEffect(id, tparams) =>
        id := Compiler scoped {
          val tps = tparams map resolveTypeParam
          BuiltinEffect(name(id), tps)
        }

      case d @ source.ExternFun(pure, id, tparams, params, ret, body) =>
        id := Compiler scoped {
          val tps = tparams map resolveTypeParam
          val ps: Params = params map resolveParamSection
          val tpe = resolveEffectful(ret)
          BuiltinFunction(name(id), tps, ps, Some(tpe), pure, body)
        }

      case d @ source.ExternInclude(path) =>
        d.contents = Compiler.resolveInclude(Compiler.source, path)
        ()
    }
  }


  /**
   * Resolve Types
   *
   * resolving a type means reconstructing the composite type (e.g. Effectful, ...) from
   * symbols, instead of trees.
   */
  def resolveValueType(tpe: source.ValueType)(given CompilerContext): ValueType = tpe match {
    case source.TypeApp(id, args) =>
      val data = id.resolveType().asValueType
      TypeApp(data, args.map(resolveValueType))
    case source.TypeVar(id) => id.resolveType().asValueType
  }

  def resolveBlockType(tpe: source.BlockType)(given CompilerContext): BlockType =
    BlockType(Nil, List(tpe.params.map(resolveValueType)), resolveEffectful(tpe.ret))

  def resolveEffect(tpe: source.Effect)(given CompilerContext): Effect =
    tpe.id.resolveType().asEffect

  def resolveEffects(tpe: source.Effects)(given CompilerContext): Effects =
    Effects(tpe.effs.map(resolveEffect))

  def resolveEffectful(e: source.Effectful)(given CompilerContext): Effectful =
    Effectful(resolveValueType(e.tpe), resolveEffects(e.eff))

  def resolveTypeParam(t: Id)(given CompilerContext): TypeVar = {
    val sym = TypeVar(t.localName)
    t := sym
    sym
  }
}


/**
 * Environment Utils -- we use a mutable cell to express adding definitions more easily
 * The kiama environment uses immutable binding since they thread the environment through
 * their attributes.
 */
trait NamerOps { self: CompilerContext =>

  // State Access
  // ============
  def source: Source = namerState.source
  def module: effekt.source.ModuleDecl = namerState.module
  def terms: Scope[TermSymbol] = namerState.terms
  def types: Scope[TypeSymbol] = namerState.types

  def (id: Id) qualifiedName: Name = QualifiedName(module.path, id.name)
  def (id: Id) localName: Name = LocalName(id.name)

  // TODO we only want to add a seed to a name under the following conditions:
  // - there is already another instance of that name in the same
  //   namespace.
  // - if it is not already fully qualified
  def (id: Id) freshTermName(qualified: Boolean = false) = {
    // how many terms of the same name are already in scope?
    val alreadyBound = terms.lookup(id.name).toList.size
    val seed = "" // if (alreadyBound > 0) "$" + alreadyBound else ""

    if (qualified) {
      QualifiedName(module.path, id.name + seed)
    } else {
      LocalName(id.name + seed)
    }
  }

  // Name Binding and Resolution
  // ===========================
  def (id: Id) := (s: TermSymbol): Unit = {
    put(id, s)
    terms.define(id.name, s)
  }

  def (id: Id) := (s: TypeSymbol): Unit = {
    put(id, s)
    types.define(id.name, s)
  }

  def bind(s: TermSymbol): Unit = terms.define(s.name.name, s)

  def bind(s: TypeSymbol): Unit = types.define(s.name.name, s)

  def bind(params: List[List[Param]]): CompilerContext = {
    params.flatten.foreach { p => bind(p) }
    this
  }

  // lookup and resolve the given id from the environment and
  // store a binding in the symbol table
  def (id: Id) resolveTerms(): List[TermSymbol] = {
    val sym = terms.lookup(id.name).getOrElse { abort(s"Could not resolve term ${id.name}") }
    put(id, sym)
    List(sym)
  }

  // for positions that do not allow overloading (for now)
  def [A](id: Id) resolveTerms(filter: PartialFunction[TermSymbol, A]): List[A] = {
    val sym = terms.lookup(id.name).getOrElse { abort(s"Could not resolve term ${id.name}") }
    put(id, sym)

    List(sym).collect(filter)
  }

  def (id: Id) resolveType(): TypeSymbol = {
    val sym = types.lookup(id.name).getOrElse { abort(s"Could not resolve type ${id.name}") }
    put(id, sym)
    sym
  }

  def scoped[R](block: => R): R = {
    val before = namerState
    namerState = before.copy(terms = before.terms.enter, types = before.types.enter)
    val result = block
    namerState = before
    result
  }
}

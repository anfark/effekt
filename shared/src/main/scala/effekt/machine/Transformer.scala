package effekt
package machine

import scala.collection.mutable

import effekt.context.Context
import effekt.context.assertions.SymbolAssertions
import effekt.symbols.{ Symbol, Name, Module, builtins }
import effekt.util.{ Task, control }
import effekt.util.control._

case class Wildcard(module: Module) extends Symbol { val name = Name("_", module) }
case class Tmp(module: Module) extends Symbol { val name = Name("tmp", module) }

class Transformer extends Phase[core.ModuleDecl, machine.ModuleDecl] {

  def run(mod: core.ModuleDecl)(implicit C: Context): Option[ModuleDecl] =
    Some(transform(mod)(TransformerContext(C)))

  def transform(mod: core.ModuleDecl)(implicit C: TransformerContext): ModuleDecl = {
    val core.ModuleDecl(path, imports, defs) = mod

    ModuleDecl(path, imports, transformToplevel(defs))
  }

  def transformToplevel(stmt: core.Stmt)(implicit C: TransformerContext): List[Decl] = {
    stmt match {
      case core.Def(blockName, core.ScopeAbs(scope, core.BlockLit(params, body)), rest) =>
        // TODO deal with evidence
        Def(blockName, scope, params.map(transform), transform(body)) :: transformToplevel(rest)
      case core.Def(blockName, core.Extern(params, body), rest) =>
        DefPrim(transform(returnTypeOf(blockName)), blockName, params.map(transform), body) :: transformToplevel(rest)
      case core.Include(content, rest) =>
        Include(content) :: transformToplevel(rest)
      case core.Exports(path, symbols) =>
        List()
      case _ =>
        println(stmt)
        C.abort("unsupported " + stmt)
    }
  }

  def transform(stmt: core.Stmt)(implicit C: TransformerContext): Stmt = {
    stmt match {
      case core.Val(name, bind, rest) =>
        Push(ValueParam(transform(C.valueTypeOf(name)), name), transform(rest), transform(bind))
      case core.Ret(expr) =>
        ANF { transform(expr).map(Ret) }
      case core.If(cond, thn, els) =>
        ANF { transform(cond).map(v => If(v, transform(thn), transform(els))) }
      case core.App(core.ScopeApp(core.BlockVar(name), scope), args) =>
        // TODO deal with evidence
        ANF { sequence(args.map(transform)).map(argVals => Jump(name, argVals)) }
      case _ =>
        println(stmt)
        C.abort("unsupported " + stmt)
    }
  }

  def transform(expr: core.Expr)(implicit C: TransformerContext): Control[Value] = {
    expr match {
      case core.IntLit(value) =>
        pure(IntLit(value))
      case core.BooleanLit(value) =>
        pure(BooleanLit(value))
      case core.ValueVar(name) =>
        pure(Var(transform(C.valueTypeOf(name)), name))
      case core.PureApp(core.BlockVar(blockName), args) => for {
        argsVals <- sequence(args.map(transform))
        result <- binding(AppPrim(transform(returnTypeOf(blockName)), blockName, argsVals))
      } yield result
      case _ =>
        println(expr)
        C.abort("unsupported " + expr)
    }
  }

  def transform(arg: core.Argument)(implicit C: TransformerContext): Control[Value] = {
    arg match {
      case expr: core.Expr =>
        transform(expr)
      case _ =>
        println(arg)
        C.abort("unsupported " + arg)
    }
  }

  def transform(param: core.Param)(implicit C: TransformerContext): Param = {
    param match {
      case core.ValueParam(name) =>
        ValueParam(transform(C.valueTypeOf(name)), name)
      case _ =>
        println(param)
        C.abort("unsupported " + param)
    }
  }

  def transform(typ: symbols.Type)(implicit C: TransformerContext): Type = {
    typ match {
      case symbols.BuiltinType(builtins.TUnit.name, List()) =>
        PrimUnit()
      case symbols.BuiltinType(builtins.TInt.name, List()) =>
        PrimInt()
      case symbols.BuiltinType(builtins.TBoolean.name, List()) =>
        PrimBoolean()
      case _ =>
        println(typ)
        C.abort("unsupported " + typ)
    }
  }

  def returnTypeOf(blockName: Symbol)(implicit C: TransformerContext): symbols.Type =
    C.blockTypeOf(blockName) match {
      case symbols.BlockType(_, _, symbols.Effectful(returnType, _)) => returnType
    }

  /**
   * Let insertion
   */

  private val delimiter: Cap[Stmt] = new Capability { type Res = Stmt }

  def ANF(e: Control[Stmt]): Stmt = control.handle(delimiter)(e).run()

  def binding(expr: AppPrim)(implicit C: TransformerContext): Control[Value] =
    control.use(delimiter) { k =>
      val x = Tmp(C.module)
      k.apply(Var(expr.typ, x)).map(body => Let(x, expr, body))
    }

  /**
   * Extra info in context
   */

  case class TransformerContext(context: Context) {
  }

  private implicit def asContext(C: TransformerContext): Context = C.context
  private implicit def getContext(implicit C: TransformerContext): Context = C.context
}

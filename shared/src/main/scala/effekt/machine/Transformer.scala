package effekt
package machine

import scala.collection.mutable

import effekt.context.Context
import effekt.context.assertions.SymbolAssertions
import effekt.symbols.{ Symbol, Name, Module }
import effekt.util.{ Task, control }
import effekt.util.control._

case class Wildcard(module: Module) extends Symbol { val name = Name("_", module) }
case class Tmp(module: Module) extends Symbol { val name = Name("tmp" + Symbol.fresh.next(), module) }

class Transformer extends Phase[core.ModuleDecl, machine.ModuleDecl] {

  def run(mod: core.ModuleDecl)(implicit C: Context): Option[ModuleDecl] =
    Some(transform(mod)(TransformerContext(C)))

  def transform(mod: core.ModuleDecl)(implicit C: TransformerContext): ModuleDecl = {
    val core.ModuleDecl(path, imports, defs) = mod

    ModuleDecl(path, imports, transform(defs))
  }

  def transform(stmt: core.Stmt)(implicit C: TransformerContext): List[Decl] = {
    stmt match {
      case core.Def(name, core.ScopeAbs(scope, core.BlockLit(params, body)), rest) =>
        Def(name, scope, params.map(transform), transformBody(body)) :: transform(rest)
      case core.Def(name, core.Extern(params, body), rest) =>
        // TODO find actual return type
        DefPrim(PrimInt(), name, params.map(transform), body) :: transform(rest)
      case core.Include(content, rest) =>
        Include(content) :: transform(rest)
      case core.Exports(path, symbols) =>
        List()
      case _ =>
        println(stmt)
        C.abort("unsupported " + stmt)
    }
  }

  def transformBody(stmt: core.Stmt)(implicit C: TransformerContext): Stmt = {
    stmt match {
      case core.Ret(expr) =>
        ANF { transform(expr).map(Ret) }
      case _ =>
        println(stmt)
        C.abort("unsupported " + stmt)
    }
  }

  def transform(expr: core.Expr)(implicit C: TransformerContext): Control[Valu] = {
    expr match {
      case core.IntLit(value) =>
        pure(IntLit(value))
      case core.PureApp(core.BlockVar(blockName), args) => for {
        argsVals <- sequence(args.map(transform))
        // TODO find out return type
        result <- binding(AppPrim(PrimInt(), blockName, argsVals))
      } yield result
      case _ =>
        println(expr)
        C.abort("unsupported " + expr)
    }
  }

  def transform(arg: core.Argument)(implicit C: TransformerContext): Control[Valu] = {
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
      case core.ValueParam(symbol) =>
        // TODO find actual parameter type
        ValueParam(PrimInt(), symbol)
      case _ =>
        println(param)
        C.abort("unsupported " + param)
    }
  }

  /**
   * Let insertion
   */

  private val delimiter: Cap[Stmt] = new Capability { type Res = Stmt }

  def ANF(e: Control[Stmt]): Stmt = control.handle(delimiter)(e).run()

  def binding(expr: AppPrim)(implicit C: TransformerContext): Control[Valu] =
    control.use(delimiter) { k =>
      val x = Tmp(C.module)
      k.apply(Var(expr.typ, x)).map(body => Let(x, expr, body))
    }

  def sequence[R](ar: List[Control[R]])(implicit C: TransformerContext): Control[List[R]] = ar match {
    case Nil => pure { Nil }
    case (r :: rs) => for {
      rv <- r
      rsv <- sequence(rs)
    } yield rv :: rsv
  }

  /**
   * Extra info in context
   */

  case class TransformerContext(context: Context) {
  }

  private implicit def asContext(C: TransformerContext): Context = C.context
  private implicit def getContext(implicit C: TransformerContext): Context = C.context
}

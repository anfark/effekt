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
      case core.Include(content, rest) =>
        Include(content) :: transform(rest)
      case core.Exports(path, symbols) =>
        List()
      case core.Def(name, core.ScopeAbs(scope, core.BlockLit(params, body)), rest) =>
        Def(name, scope, params.map(transform), transformBody(body)) :: transform(rest)
      case _ =>
        println(stmt)
        C.abort("unsupported " + stmt)
    }
  }

  def transformBody(stmt: core.Stmt)(implicit C: TransformerContext): Stmt = {
    stmt match {
      case core.Ret(core.IntLit(value)) =>
        Ret(IntLit(value))
      case _ =>
        println(stmt)
        C.abort("unsupported " + stmt)
    }
  }

  def transform(param: core.Param)(implicit C: TransformerContext): Param = {
    param match {
      case core.ValueParam(symbol) =>
        ValueParam(symbol)
      case _ =>
        println(param)
        C.abort("unsupported " + param)
    }
  }

  case class TransformerContext(context: Context) {
  }

  private implicit def asContext(C: TransformerContext): Context = C.context
  private implicit def getContext(implicit C: TransformerContext): Context = C.context
}

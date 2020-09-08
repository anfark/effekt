package effekt
package machine

import scala.collection.mutable

import effekt.context.Context
import effekt.context.assertions.SymbolAssertions
import effekt.symbols._
import effekt.util.{ Task, control }
import effekt.util.control._

case class Wildcard(module: Module) extends Symbol { val name = Name("_", module) }
case class Tmp(module: Module) extends Symbol { val name = Name("tmp" + Symbol.fresh.next(), module) }

class Transformer extends Phase[core.ModuleDecl, machine.ModuleDecl] {

  def run(mod: core.ModuleDecl)(implicit C: Context): Option[ModuleDecl] =
    Some(transform(mod)(TransformerContext(C)))

  def transform(mod: core.ModuleDecl)(implicit C: TransformerContext): ModuleDecl = {
    val core.ModuleDecl(path, imports, defs) = mod

    ModuleDecl(path, imports, Exports(path, List())).inheritPosition(mod)
  }

  case class TransformerContext(context: Context) {
  }

  private implicit def asContext(C: TransformerContext): Context = C.context
  private implicit def getContext(implicit C: TransformerContext): Context = C.context
}

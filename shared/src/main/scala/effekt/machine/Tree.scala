package effekt
package machine

import effekt.context.Context
import effekt.symbols.{ NoName, QualifiedName, Symbol }

sealed trait Tree extends Product {
  def inheritPosition(from: source.Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(path: String, imports: List[String], decls: List[Decl]) extends Tree

sealed trait Param extends Tree { def id: Symbol }
case class ValueParam(id: Symbol) extends Param

/**
 * Toplevel declarations
 */
sealed trait Decl extends Tree

case class Def(id: Symbol, scope: Symbol, params: List[Param], body: Stmt) extends Decl

case class Include(contents: String) extends Decl

/**
 * Statements
 */
sealed trait Stmt extends Tree

case class Ret(e: Expr) extends Stmt

/**
 * Fine-grain CBV: Arguments can be either expressions or blocks
 */
sealed trait Argument extends Tree

/**
 * Expressions
 */
sealed trait Expr extends Tree with Argument

sealed trait Literal[T] extends Expr {
  def value: T
}
case class IntLit(value: Int) extends Literal[Int]


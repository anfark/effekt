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
case class DefPrim(id: Symbol, params: List[Param], body: String) extends Decl
case class Include(contents: String) extends Decl

/**
 * Statements
 */
sealed trait Stmt extends Tree

case class Let(id: Symbol, bind: Expr, body: Stmt) extends Stmt
case class Ret(v: Valu) extends Stmt

/**
 * Expressions
 */
sealed trait Expr extends Tree

case class AppPrim(name: Symbol, args: List[Valu]) extends Expr

/**
 * Values
 */
sealed trait Valu extends Tree

case class IntLit(value: Int) extends Valu
case class Var(id: Symbol) extends Valu


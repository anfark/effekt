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

/**
 * Toplevel declarations
 */
sealed trait Decl extends Tree

case class Def(id: Symbol, scope: Symbol, params: List[Param], body: Stmt) extends Decl
case class DefPrim(typ: Type, id: Symbol, params: List[Param], body: String) extends Decl
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

case class AppPrim(typ: Type, name: Symbol, args: List[Valu]) extends Expr

/**
 * Values
 */
sealed trait Valu extends Tree

case class IntLit(value: Int) extends Valu
case class Var(typ: Type, id: Symbol) extends Valu

/**
 * Parameters
 */
sealed trait Param extends Tree

case class ValueParam(typ: Type, id: Symbol) extends Param

/**
 * Types
 */
sealed trait Type extends Tree

case class PrimInt() extends Type


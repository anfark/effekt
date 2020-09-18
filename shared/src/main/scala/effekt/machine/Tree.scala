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
// Instructions
case class Let(id: Symbol, bind: Expr, body: Stmt) extends Stmt
case class Push(param: ValueParam, body: Stmt, rest: Stmt) extends Stmt
// Terminators
case class Ret(v: Value) extends Stmt
case class Jump(id: Symbol, args: List[Value]) extends Stmt
case class If(cond: Value, thn: Stmt, els: Stmt) extends Stmt

/**
 * Expressions
 */
sealed trait Expr extends Tree

case class AppPrim(typ: Type, id: Symbol, args: List[Value]) extends Expr

/**
 * Values
 */
sealed trait Value extends Tree

case class IntLit(value: Int) extends Value
case class BooleanLit(value: Boolean) extends Value
case class Var(typ: Type, id: Symbol) extends Value

/**
 * Parameters
 */
sealed trait Param extends Tree

case class ValueParam(typ: Type, id: Symbol) extends Param

/**
 * Types
 */
sealed trait Type extends Tree

case class PrimUnit() extends Type
case class PrimInt() extends Type
case class PrimBoolean() extends Type


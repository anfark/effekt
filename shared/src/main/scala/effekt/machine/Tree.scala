package effekt
package machine

import effekt.context.Context
import effekt.symbols.{ NoName, QualifiedName, ValueSymbol, BlockSymbol }

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

case class Def(id: BlockSymbol, block: BlockLit) extends Decl
case class DefPrim(typ: Type, id: BlockSymbol, params: List[ValueParam], body: String) extends Decl
case class Include(contents: String) extends Decl

/**
 * Statements
 */
sealed trait Stmt extends Tree
// Instructions
case class Let(id: ValueSymbol, bind: Expr, rest: Stmt) extends Stmt
case class Push(param: ValueParam, body: Stmt, rest: Stmt) extends Stmt
case class DefLocal(id: BlockSymbol, block: BlockLit, rest: Stmt) extends Stmt
// Terminators
case class Ret(v: Value) extends Stmt
case class Jump(id: BlockSymbol, args: List[Value]) extends Stmt
case class If(cond: Value, thenBlock: BlockSymbol, elseBlock: BlockSymbol) extends Stmt

/**
 * Expressions
 */
sealed trait Expr extends Tree

case class AppPrim(typ: Type, id: BlockSymbol, args: List[Value]) extends Expr

/**
 * Blocks
 */
case class BlockLit(params: List[ValueParam], body: Stmt)

/**
 * Values
 */
sealed trait Value extends Tree

case class IntLit(value: Int) extends Value
case class BooleanLit(value: Boolean) extends Value
case class Var(typ: Type, id: ValueSymbol) extends Value

/**
 * Parameters
 */
case class ValueParam(typ: Type, id: ValueSymbol)

/**
 * Types
 */
sealed trait Type extends Tree

case class PrimUnit() extends Type
case class PrimInt() extends Type
case class PrimBoolean() extends Type


package effekt
package machine

import effekt.context.Context
import effekt.symbols.{ NoName, QualifiedName, Symbol, ValueSymbol, BlockSymbol }

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
// TODO is it sensible for prims to accept blocks?
case class DefPrim(typ: Type, id: BlockSymbol, params: List[Param], body: String) extends Decl
case class Include(contents: String) extends Decl

/**
 * Statements
 */
sealed trait Stmt extends Tree
// Instructions
case class Let(id: ValueSymbol, bind: Expr, rest: Stmt) extends Stmt
case class DefLocal(id: BlockSymbol, block: BlockLit, rest: Stmt) extends Stmt
case class PushFrame(cntType: List[Type], id: BlockSymbol, args: List[Value], rest: Stmt) extends Stmt
case class NewStack(cntType: List[Type], id: BlockSymbol, blockName: BlockSymbol, args: List[Value], rest: Stmt) extends Stmt
case class PushStack(stack: Value, rest: Stmt) extends Stmt
case class PopStack(id: BlockSymbol, rest: Stmt) extends Stmt
// Terminators
case class Ret(values: List[Value]) extends Stmt
case class Jump(id: BlockSymbol, args: List[Value]) extends Stmt
case class JumpLocal(id: BlockSymbol, args: List[Value]) extends Stmt
case class If(cond: Value, thenBlock: BlockSymbol, thenArgs: List[Value], elseBlock: BlockSymbol, elseArgs: List[Value]) extends Stmt

/**
 * Expressions
 */
sealed trait Expr extends Tree

case class AppPrim(typ: Type, id: BlockSymbol, args: List[Value]) extends Expr

/**
 * Blocks
 */
case class BlockLit(params: List[Param], body: Stmt)

/**
 * Values
 */
sealed trait Value extends Tree

case class IntLit(value: Int) extends Value
case class BooleanLit(value: Boolean) extends Value
// Refers to values and stack are values
// TODO Change this symbol back to value symbol.... but what about block parameters?
case class Var(typ: Type, id: Symbol) extends Value

/**
 * Parameters
 */
case class Param(typ: Type, id: Symbol) extends Tree

/**
 * Types
 */
sealed trait Type extends Tree

case class PrimUnit() extends Type
case class PrimInt() extends Type
case class PrimBoolean() extends Type
case class Stack(cntType: List[Type]) extends Type


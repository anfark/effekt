package effekt
package machine

import scala.collection.mutable

import effekt.context.Context
import effekt.context.assertions.SymbolAssertions
import effekt.symbols.{ Symbol, ValueSymbol, BlockSymbol, Name, Module, builtins }
import effekt.util.{ Task, control }
import effekt.util.control._

case class FreshValueSymbol(baseName: String, module: Module) extends ValueSymbol {
  val name = Name(baseName, module)
}
case class FreshBlockSymbol(baseName: String, module: Module) extends BlockSymbol {
  val name = Name(baseName, module)
}

class Transformer extends Phase[core.ModuleDecl, machine.ModuleDecl] {

  def run(mod: core.ModuleDecl)(implicit C: Context): Option[ModuleDecl] =
    Some(transform(mod)(TransformerContext(C)))

  def transform(mod: core.ModuleDecl)(implicit C: TransformerContext): ModuleDecl = {
    val core.ModuleDecl(path, imports, defs) = mod

    ModuleDecl(path, imports, transformToplevel(defs))
  }

  def transformToplevel(stmt: core.Stmt)(implicit C: TransformerContext): List[Decl] = {
    stmt match {
      case core.Def(blockName: BlockSymbol, core.ScopeAbs(scope, core.BlockLit(params, body)), rest) => {
        // TODO make core.Def always contain BlockSymbol and also the others
        // TODO deal with evidence
        val entryBlockSymbol = FreshBlockSymbol("entry", C.module);
        Def(blockName, BlockLit(
          params.map(transform),
          DefLocal(entryBlockSymbol, BlockLit(List(), transform(body)),
            Jump(entryBlockSymbol, List()))
        )) ::
          transformToplevel(rest)
      }
      case core.Def(blockName: BlockSymbol, core.Extern(params, body), rest) =>
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
      case core.Val(name: ValueSymbol, bind, rest) =>
        val frameName = FreshBlockSymbol("f", C.module);
        DefLocal(
          frameName,
          BlockLit(List(transform(core.ValueParam(name))), transform(rest)),
          Push(transform(C.valueTypeOf(name)), frameName, transform(bind))
        )
      case core.Ret(expr) =>
        ANF { transform(expr).map(Ret) }
      case core.Def(blockName: BlockSymbol, core.ScopeAbs(scope, block), rest) =>
        // TODO deal with evidence
        DefLocal(blockName, transform(block), transform(rest))
      case core.App(core.ScopeApp(core.BlockVar(name: BlockSymbol), scope), args) =>
        // TODO deal with evidence
        ANF { sequence(args.map(transform)).map(argVals => Jump(name, argVals)) }
      case core.If(cond, thenStmt, elseStmt) => {
        val thenBlockName = FreshBlockSymbol("then", C.module);
        val elseBlockName = FreshBlockSymbol("else", C.module);
        DefLocal(thenBlockName, BlockLit(List(), transform(thenStmt)),
          DefLocal(elseBlockName, BlockLit(List(), transform(elseStmt)),
            ANF { transform(cond).map(v => If(v, thenBlockName, elseBlockName)) }))
      }
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
      case core.ValueVar(name: ValueSymbol) =>
        pure(Var(transform(C.valueTypeOf(name)), name))
      case core.PureApp(core.BlockVar(blockName: BlockSymbol), args) => for {
        argsVals <- sequence(args.map(transform))
        result <- binding(AppPrim(transform(returnTypeOf(blockName)), blockName, argsVals))
      } yield result
      case _ =>
        println(expr)
        C.abort("unsupported " + expr)
    }
  }

  def transform(block: core.Block)(implicit C: TransformerContext): BlockLit = {
    block match {
      case core.BlockLit(params, body) =>
        BlockLit(params.map(transform), transform(body))
      case _ =>
        println(block)
        C.abort("unsupported " + block)
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

  def transform(param: core.Param)(implicit C: TransformerContext): ValueParam = {
    param match {
      case core.ValueParam(name: ValueSymbol) =>
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
      val x = FreshValueSymbol("x", C.module)
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

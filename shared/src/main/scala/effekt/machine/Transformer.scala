package effekt
package machine

import scala.collection.mutable

import effekt.context.Context
import effekt.context.assertions.SymbolAssertions
import effekt.symbols.{ Symbol, UserEffect, ValueSymbol, BlockSymbol, Name, Module, builtins, / }
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
        C.localDefsSet = Set();
        C.blockParamsSet = Set();
        params.foreach {
          case core.BlockParam(name: BlockSymbol) => C.blockParamsSet += name
          case _ => ()
        };
        Def(blockName, BlockLit(params.map(transform), transform(body))) ::
          transformToplevel(rest)
      }
      case core.Def(blockName: BlockSymbol, core.Extern(params, body), rest) =>
        DefPrim(transform(returnTypeOf(blockName)), blockName, params.map(transform), body) :: transformToplevel(rest)
      case core.Include(content, rest) =>
        Include(content) :: transformToplevel(rest)
      case core.Record(_, _, rest) =>
        // TODO these are for records and capabilities
        // TODO We only support singleton capabilities
        transformToplevel(rest)
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
        C.localDefsSet += frameName;
        DefLocal(
          frameName,
          BlockLit(List(transform(core.ValueParam(name))), transform(rest)),
          PushFrame(List(transform(C.valueTypeOf(name))), frameName, List(), transform(bind))
        )
      case core.Ret(expr) =>
        ANF { transform(expr).map(x => Ret(List(x))) }
      case core.Def(blockName: BlockSymbol, core.ScopeAbs(scope, core.BlockLit(params, body)), rest) =>
        // TODO deal with evidence
        C.localDefsSet += blockName;
        params.foreach {
          case core.BlockParam(name: BlockSymbol) => C.blockParamsSet += name
          case _ => ()
        };
        DefLocal(blockName, BlockLit(params.map(transform(_)), transform(body)),
          transform(rest))
      case core.App(core.ScopeApp(core.BlockVar(name: BlockSymbol), scope), args) =>
        // TODO deal with evidence
        ANF {
          sequence(args.map(transform)).map(argVals =>
            if (C.localDefsSet.contains(name)) {
              JumpLocal(name, argVals)
            } else {
              if (C.blockParamsSet.contains(name)) {
                PushStack(Var(transform(C.blockTypeOf(name)), name), Ret(argVals))
              } else {
                Jump(name, argVals)
              }
            })
        }
      case core.App(core.Member(core.ScopeApp(core.BlockVar(name: UserEffect), _), _), args) => {
        // TODO deal with evidence
        // TODO merge this with other application case
        ANF {
          sequence(args.map(transform)).map(argVals =>
            PushStack(Var(transform(capabilityTypeOf(name)), name), Ret(argVals)))
        }
      }
      case core.If(cond, thenStmt, elseStmt) => {
        val thenBlockName = FreshBlockSymbol("then", C.module);
        C.localDefsSet += thenBlockName;
        val elseBlockName = FreshBlockSymbol("else", C.module);
        C.localDefsSet += elseBlockName;
        DefLocal(thenBlockName, BlockLit(List(), transform(thenStmt)),
          DefLocal(elseBlockName, BlockLit(List(), transform(elseStmt)),
            ANF { transform(cond).map(v => If(v, thenBlockName, List(), elseBlockName, List())) }))
      }
      case core.Handle(core.ScopeAbs(_, body), handlers) => {
        // TODO deal with evidence
        // TODO factor name generation, binding out and use better names
        val answerType = transform(answerTypeOf(handlers));
        val retName = FreshBlockSymbol("r", C.module);
        val parName = FreshValueSymbol("a", C.module);
        val delName = FreshBlockSymbol("k", C.module);
        val bodyName = FreshBlockSymbol("body", C.module);
        C.localDefsSet += retName;
        C.blockParamsSet += delName;
        C.localDefsSet += bodyName;
        DefLocal(retName, BlockLit(
          List(Param(answerType, parName)),
          Ret(List(Var(answerType, parName)))
        ),
          NewStack(List(answerType), delName, retName, List(),
            PushStack(
              Var(Stack(List(answerType)), delName),
              DefLocal(bodyName, transform(body),
                ANF {
                  sequence(handlers.map(transform).map(bindingStack)).map { args =>
                    JumpLocal(bodyName, args)
                  }
                })
            )))
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
      case core.UnitLit() =>
        pure(UnitLit())
      case core.ValueVar(name: ValueSymbol) =>
        pure(Var(transform(C.valueTypeOf(name)), name))
      case core.PureApp(core.BlockVar(blockName: BlockSymbol), args) => for {
        argsVals <- sequence(args.map(transform))
        result <- bindingValue(AppPrim(transform(returnTypeOf(blockName)), blockName, argsVals))
      } yield result
      case _ =>
        println(expr)
        C.abort("unsupported " + expr)
    }
  }

  def transform(block: core.Block)(implicit C: TransformerContext): BlockLit = {
    block match {
      case core.BlockLit(params, body) =>
        params.foreach {
          case core.BlockParam(name: BlockSymbol) => C.blockParamsSet += name
          case _ => ()
        };
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
      case core.ScopeAbs(_, block: BlockLit) =>
        // TODO This seems to overlap, move this elsewhere?
        bindingStack(transform(block))
      case _ =>
        println(arg)
        C.abort("unsupported " + arg)
    }
  }

  def transform(handler: core.Handler)(implicit C: TransformerContext): BlockLit = {
    handler match {
      case core.Handler(_, List((_, core.BlockLit(params :+ resume, body)))) =>
        // TODO we assume here that resume is the last param
        // TODO we assume that there are no block params in handlers
        val resumeName = resume.id.asInstanceOf[BlockSymbol]
        C.blockParamsSet += resumeName;
        BlockLit(
          params.map(transform(_)),
          PopStack(resumeName, transform(body))
        )
      case _ =>
        println(handler)
        C.abort("unsupported " + handler)
    }
  }

  def transform(param: core.Param)(implicit C: TransformerContext): Param = {
    param match {
      case core.ValueParam(name: ValueSymbol) =>
        Param(transform(C.valueTypeOf(name)), name)
      case core.BlockParam(name: BlockSymbol) =>
        Param(transform(C.blockTypeOf(name)), name)
      case core.BlockParam(name: UserEffect) =>
        Param(transform(capabilityTypeOf(name)), name)
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
      case symbols.BlockType(_, sections, ret / _) =>
        // TODO do we only use this function on parameter types?
        // TODO capabilities?
        Stack(sections.flatten.map(transform(_)))
      case _ =>
        println(typ)
        C.abort("unsupported " + typ)
    }
  }

  def returnTypeOf(blockName: Symbol)(implicit C: TransformerContext): symbols.Type =
    C.blockTypeOf(blockName) match {
      case symbols.BlockType(_, _, symbols.Effectful(returnType, _)) => returnType
    }

  def answerTypeOf(handlers: List[core.Handler])(implicit C: TransformerContext): symbols.Type =
    handlers match {
      case core.Handler(_, List((_, core.BlockLit(params, _)))) :: _ =>
        // TODO we assume here that resume is the last param
        returnTypeOf(params.last.id)
      case _ =>
        println(handlers)
        C.abort("can't find answer type of " + handlers)
    }

  def capabilityTypeOf(name: UserEffect)(implicit C: TransformerContext): symbols.Type = {
    name.ops match {
      case List(op) =>
        C.blockTypeOf(op)
      case _ =>
        println(name.ops)
        C.abort("unsupported " + name.ops)
    }
  }

  /**
   * Let insertion
   */

  private val delimiter: Cap[Stmt] = new Capability { type Res = Stmt }

  def ANF(e: Control[Stmt]): Stmt = control.handle(delimiter)(e).run()

  def bindingValue(expr: AppPrim)(implicit C: TransformerContext): Control[Value] =
    control.use(delimiter) { resume =>
      val x = FreshValueSymbol("x", C.module)
      resume.apply(Var(expr.typ, x)).map(rest => Let(x, expr, rest))
    }

  def bindingStack(block: BlockLit)(implicit C: TransformerContext): Control[Value] =
    control.use(delimiter) { resume =>
      val f = FreshBlockSymbol("f", C.module)
      val k = FreshBlockSymbol("k", C.module)
      // TODO add to localDefsSet and blockParamsSet?
      block match {
        case BlockLit(params, body) =>
          val cntType = params.map(_.typ);
          resume.apply(Var(Stack(cntType), k)).map(rest =>
            DefLocal(f, BlockLit(params, body),
              NewStack(cntType, k, f, List(), rest)))
      }
    }

  /**
   * Extra info in context
   */

  case class TransformerContext(context: Context) {
    var localDefsSet: Set[BlockSymbol] = Set()
    var blockParamsSet: Set[BlockSymbol] = Set()
  }

  private implicit def asContext(C: TransformerContext): Context = C.context
  private implicit def getContext(implicit C: TransformerContext): Context = C.context
}

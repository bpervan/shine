package LowLevelCombinators

import Core.OperationalSemantics._
import Core._
import DSL.typed._
import apart.arithmetic.{ArithExpr, NamedVar, RangeAdd}
import opencl.generator.OpenCLAST.Block

import scala.xml.Elem

case class For(n: ArithExpr,
               body: Phrase[ExpType -> CommandType])
  extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    body checkType t"exp[$int] -> comm"
  }

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, n)
    val bodyE = OperationalSemantics.eval(s, body)
    (0 until nE.eval).foldLeft(s)( (s1, i) => {
      OperationalSemantics.eval(s1, bodyE(LiteralPhrase(i)))
    } )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    For(fun(n), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String = s"(for 0..$n ${PrettyPrinter(body)})"

  override def xmlPrinter: Elem =
    <for n={ToString(n)}>
      {Core.xmlPrinter(body)}
    </for>

  override def toOpenCL(block: Block, env: ToOpenCL.Environment): Block = {
    import opencl.generator.OpenCLAST._

    val name = newName()

    env.ranges(name) = RangeAdd(0, n, 1)

    val init = VarDecl(name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(name),
      ArithExpression(n),
      CondExpression.Operator.<)

    val v = NamedVar(name)
    val increment = AssignmentExpression(ArithExpression(v), ArithExpression(v + 1))

    val bodyE = Lift.liftFunction(body)
    val i = identifier(name, ExpType(int))

    val body_ = ToOpenCL.cmd(bodyE(i), Block(), env)

    (block: Block) += ForLoop(init, cond, increment, body_)

    env.ranges.remove(name)

    block
  }
}
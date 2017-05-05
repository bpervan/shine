package idealised.LowLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.DSL.typed._

import scala.xml.Elem

final case class New(dt: DataType,
                     addressSpace: AddressSpace,
                     f: Phrase[(ExpType x AccType) -> CommandType])
  extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt: DataType) -> /* (addressSpace: AddressSpace) -> */
      (f :: t"var[$dt] -> comm") -> comm
  }

  override def eval(s: Store): Store = {
    val f_ = OperationalSemantics.eval(s, f)
    val arg = identifier(newName(), f.t.inT)
    val newStore = OperationalSemantics.eval(s + (arg.name -> 0), f_(arg))
    newStore - arg.name
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    New(fun(dt), addressSpace, VisitAndRebuild(f, fun))
  }

  override def prettyPrint: String = s"(new $addressSpace ${PrettyPrinter(f)})"

  override def xmlPrinter: Elem =
    <new dt={ToString(dt)} addressspace={ToString(addressSpace)}>
      {Core.xmlPrinter(f)}
    </new>
}
package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.macros.Primitive.comPrimitive

@comPrimitive
case class Skip() extends CommandPrimitive {
  override def eval(s: Store): Store = s
}

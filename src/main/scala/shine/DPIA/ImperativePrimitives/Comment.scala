package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases.CommandPrimitive
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class Comment(comment : String) extends CommandPrimitive
{
  override def eval(s: Store): Store = s
  override def prettyPrint: String = s"\n//$comment\n"
}

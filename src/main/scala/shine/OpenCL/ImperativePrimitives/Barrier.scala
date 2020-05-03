package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases.CommandPrimitive
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class Barrier(local: Boolean,
                         global: Boolean) extends CommandPrimitive
{
  override def prettyPrint: String =
    s"""barrier(
        ${if(local) "CLK_LOCAL_MEM_FENCE" else ""}
        ${if(global && local) "|" else ""}
        ${if(global) "CLK_GLOBAL_MEM_FENCE" else ""})"""

  override def eval(s: Store): Store = s
}

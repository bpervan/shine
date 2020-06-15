package shine.OpenCL.Primitives.OpenCL

import shine.DPIA.DSL.identifier
import shine.DPIA.Phrases.{CommandPrimitive, Phrase}
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.{AddressSpace, CommType, DataType, comm}
import shine.DPIA.{->:, VarType, freshName, varT}
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class New(a: AddressSpace,
                     dt: DataType,
                     f: Phrase[VarType ->: CommType])
  extends CommandPrimitive
{
  f :: varT(dt) ->: comm

  override def eval(s: Store): Store = {
    val f_ = OperationalSemantics.eval(s, f)
    val arg = identifier(freshName("x"), f.t.inT)
    val newStore = OperationalSemantics.eval(s + (arg.name -> 0), f_(arg))
    newStore - arg.name
  }
}

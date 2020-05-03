package shine.DPIA.ImperativePrimitives

import shine.DPIA.FunctionalPrimitives.AsIndex
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class For(n: Nat,
                     loopBody: Phrase[ExpType ->: CommType],
                     unroll: Boolean) extends CommandPrimitive
{
  loopBody :: expT(idx(n), read) ->: comm

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, AsIndex(n, Natural(n)))
    val bodyE = OperationalSemantics.eval(s, loopBody)
    (0 until nE.eval).foldLeft(s)((s1, i) =>
      OperationalSemantics.eval(s1, bodyE(Literal(i)))
    )
  }
}

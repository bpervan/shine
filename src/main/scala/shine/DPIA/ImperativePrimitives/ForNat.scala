package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ForNat(n: Nat,
                        loopBody: Phrase[`(nat)->:`[CommType]],
                        unroll:Boolean) extends CommandPrimitive
{
  loopBody :: loopBody.t.x ->: comm
}

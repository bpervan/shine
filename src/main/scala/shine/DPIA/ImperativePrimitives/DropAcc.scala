package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

// this drops n many elements from an array of m elements
@accPrimitive
final case class DropAcc(n: Nat,
                         m: Nat,
                         dt: DataType,
                         array: Phrase[AccType]) extends AccPrimitive
{
  array :: accT({n + m}`.`dt)
  override val t: AccType = accT({m - n}`.`dt)
}

package shine.OpenCL.Primitives.OpenCL

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.ParallelismLevel
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class IdxDistributeAcc(m: Nat,
                                  n: Nat,
                                  stride: Nat,
                                  parallelismLevel: ParallelismLevel,
                                  dt: DataType,
                                  array: Phrase[AccType]) extends AccPrimitive
{
  array :: accT(m`.`dt)
  override val t: AccType = accT(n`.`dt)
}

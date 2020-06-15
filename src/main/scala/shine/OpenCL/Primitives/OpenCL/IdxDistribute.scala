package shine.OpenCL.Primitives.OpenCL

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Phrases.{ExpPrimitive, Phrase}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, read, _}
import shine.DPIA.{->:, Nat, expT}
import shine.OpenCL.ParallelismLevel
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class IdxDistribute(m: Nat,
                               n: Nat,
                               stride: Nat,
                               parallelismLevel: ParallelismLevel,
                               dt: DataType,
                               array: Phrase[ExpType]) extends ExpPrimitive
{
  array :: expT(m`.`dt, read)
  override val t: ExpType = expT(n`.`dt, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    throw new Exception("This should never happen.")

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    throw new Exception("This should never happen.")
}

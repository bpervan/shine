package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.AsVectorAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class AsVectorAligned(n: Nat,
                                 m: Nat,
                                 w: AccessType,
                                 dt: ScalarType,
                                 array: Phrase[ExpType]
                                ) extends ExpPrimitive with AccT {
  array :: expT((m * n)`.`dt, w)
  override val t: ExpType = expT(m`.`vec(n, dt), w)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    acc(array)(AsVectorAcc(n, m, dt, A))
}

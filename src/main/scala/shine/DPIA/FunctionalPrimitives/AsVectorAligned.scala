package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.AsVectorAcc
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class AsVectorAligned(n: Nat,
                                 m: Nat,
                                 dt: ScalarType,
                                 array: Phrase[ExpType])
  extends ExpPrimitive
{
  array :: expT({m * n}`.`dt, read)
  override val t: ExpType = expT(m`.`vec(n, dt), read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    acc(array)(AsVectorAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    con(array)(λ(array.t)(x => C(AsVectorAligned(n, m, dt, x)) ))
  }
}

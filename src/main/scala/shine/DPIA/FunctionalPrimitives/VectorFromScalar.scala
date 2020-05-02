package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

import scala.language.reflectiveCalls

@expPrimitive
final case class VectorFromScalar(n: Nat,
                                  dt: ScalarType,
                                  arg: Phrase[ExpType]) extends ExpPrimitive
{
  arg :: expT(dt, read)
  override val t: ExpType = expT(vec(n, dt), read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    TranslationToImperative.con(arg)(λ(expT(dt, read))(e =>
      A :=|VectorType(n, dt)| VectorFromScalar(n, dt, e) ))

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(arg)(λ(expT(dt, read))(e =>
      C(VectorFromScalar(n, dt, e)) ))
}

package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, _}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

import scala.language.reflectiveCalls

@expPrimitive
final case class IndexAsNat(n: Nat, e: Phrase[ExpType]) extends ExpPrimitive
{
  e :: expT(idx(n), read)
  override val t: ExpType = expT(NatType, read)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext
                         ): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(expT(idx(n), read))(x =>
      A :=|NatType| IndexAsNat(n, x)))
  }

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext
                             ): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(expT(idx(n), read))(x =>
      C(IndexAsNat(n, x))))
  }
}

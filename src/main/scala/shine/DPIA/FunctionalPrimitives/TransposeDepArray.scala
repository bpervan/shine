package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class TransposeDepArray(n:Nat,
                                   m:Nat,
                                   f:NatToData,
                                   array:Phrase[ExpType]) extends ExpPrimitive
{
  array :: expT(n`.`(m`.d`f), read)
  override val t: ExpType = expT(m`.d`{ k => n`.`f(k) }, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(array)(λ(expT(n`.`(m`.d`f), read))(x =>
      C(TransposeDepArray(n, m, f, x))))
}

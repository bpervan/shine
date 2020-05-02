package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.{ExpPrimitive, Phrase}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types.{AccType, CommType, ExpType, _}
import shine.DPIA.{->:, Nat, _}
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Pad(n: Nat,
                     l: Nat,
                     r: Nat,
                     dt: DataType,
                     padExp: Phrase[ExpType],
                     array: Phrase[ExpType]) extends ExpPrimitive
{
  padExp :: expT(dt, read)
  array :: expT(n`.`dt, read)
  override val t: ExpType = expT({l + n + r}`.`dt, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(expT(n`.`dt, read))(x =>
      con(padExp)(λ(expT(dt, read))(p =>
        C(Pad(n, l, r, dt, p, x))))))
  }
}

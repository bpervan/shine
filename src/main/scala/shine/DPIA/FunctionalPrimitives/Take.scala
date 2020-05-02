package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.{ExpPrimitive, Phrase}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, _}
import shine.DPIA.{->:, Nat, _}
import shine.macros.Primitive.expPrimitive

// this takes n many elements from an array of n + m elements
@expPrimitive
final case class Take(n: Nat,
                      m: Nat,
                      w: AccessType,
                      dt: DataType,
                      array: Phrase[ExpType]) extends ExpPrimitive
{
  array :: expT({n + m}`.`dt, w)
  override val t: ExpType = expT(n`.`dt, w)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(array)(λ(expT({n + m}`.`dt, read))(x =>
      C(Take(n, m, w, dt, x))))
}

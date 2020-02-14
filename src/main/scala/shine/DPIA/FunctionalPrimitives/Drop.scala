package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.{ExpPrimitive, Phrase}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, _}
import shine.DPIA.{->:, Nat, _}
import shine.macros.Primitive.expPrimitive

// this drops n many elements from an array of n + m elements
@expPrimitive
final case class Drop(n: Nat,
                      m: Nat,
                      w: AccessType,
                      dt: DataType,
                      array: Phrase[ExpType])
  extends ExpPrimitive
{
  array :: expT({n + m}`.`dt, w)
  override val t: ExpType = expT(m`.`dt, w)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    con(array)(λ(expT({n + m}`.`dt, read))(x => C(Drop(n, m, read, dt, x))))
  }
}

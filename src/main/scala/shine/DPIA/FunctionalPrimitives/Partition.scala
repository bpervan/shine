package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Partition(n: Nat,
                           m: Nat,
                           lenF: NatToNat,
                           dt: DataType,
                           array: Phrase[ExpType]) extends ExpPrimitive
{
  array :: expT(n`.`dt, read)
  override val t: ExpType = expT(m`.d`{ i => lenF(i)`.`dt }, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(expT(n`.`dt, read))(x => C(Partition(n, m, lenF, dt, x)) ))
  }
}
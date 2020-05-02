package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.TransposeAcc
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Transpose(n: Nat, m: Nat, dt: DataType,
                           array: Phrase[ExpType]) extends ExpPrimitive
{
  array :: expT(n`.`(m`.`dt), read)
  override val t: ExpType = expT(m`.`(n`.`dt), read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    TranslationToImperative.acc(array)(TransposeAcc(n, m, dt, A))

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(array)(fun(array.t)(x =>
      C(Transpose(n, m, dt, x))))

  override def fedeTranslation(env: Predef.Map[Identifier[ExpType],
                               Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]
                              ): Phrase[AccType] =
    TranslationToImperative.fedAcc(env)(array)(
      fun(AccType(C.t.inT.dataType))(o => TransposeAcc(n, m, dt, C(o))))
}

package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.{ZipAcc1, ZipAcc2}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Zip(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     e1: Phrase[ExpType],
                     e2: Phrase[ExpType]) extends ExpPrimitive
{
  e1 :: expT(n`.`dt1, read)
  e2 :: expT(n`.`dt2, read)
  override val t: ExpType = expT(n`.`(dt1 x dt2), read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    TranslationToImperative.acc(e1)(ZipAcc1(n, dt1, dt2, A)) `;`
      TranslationToImperative.acc(e2)(ZipAcc2(n, dt1, dt2, A))

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(e1)(λ(expT(n`.`dt1, read))(x =>
      TranslationToImperative.con(e2)(λ(expT(n`.`dt2, read))(y =>
        C(Zip(n, dt1, dt2, x, y)) )) ))

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, e1), OperationalSemantics.eval(s, e2)) match {
      case (ArrayData(lhsE), ArrayData(rhsE)) =>
        ArrayData((lhsE zip rhsE) map { p =>
          PairData(p._1, p._2)
        })

      case _ => throw new Exception("This should not happen")
    }
  }
}
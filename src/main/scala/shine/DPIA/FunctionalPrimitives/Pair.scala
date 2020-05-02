package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Pair(dt1: DataType,
                      dt2: DataType,
                      fst: Phrase[ExpType],
                      snd: Phrase[ExpType]) extends ExpPrimitive
{
  fst :: expT(dt1, read)
  snd :: expT(dt2, read)
  override val t: ExpType = expT(dt1 x dt2, read)

  override def eval(s: Store): Data = {
    PairData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    import TranslationToImperative._

    acc(fst)(pairAcc1(dt1, dt2, A)) `;`
      acc(snd)(recordAcc2(dt1, dt2, A))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    import TranslationToImperative._

    con(fst)(λ(expT(dt1, read))(x =>
      con(snd)(λ(expT(dt2, read))(y =>
        C(Pair(dt1, dt2, x, y)) )) ))
  }
}

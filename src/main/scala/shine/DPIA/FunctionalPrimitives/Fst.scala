package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

import scala.language.reflectiveCalls

@expPrimitive
final case class Fst(dt1: DataType,
                     dt2: DataType,
                     pair: Phrase[ExpType]) extends ExpPrimitive
{
  pair :: expT(dt1 x dt2, read)
  override val t: ExpType = expT(dt1, read)

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, pair) match {
      case r: PairData => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    //TODO Assignments for general types should not be allowed, making this definition invalid
    dt1 match { case _ : BasicType => ; case _ => error(s"$dt1 assign") }
    con(pair)(λ(expT(dt1 x dt2, read))(x => A :=|dt1| Fst(dt1, dt2, x)) )
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(pair)(λ(expT(dt1 x dt2, read))(x => C(Fst(dt1, dt2, x)) ))
  }
}

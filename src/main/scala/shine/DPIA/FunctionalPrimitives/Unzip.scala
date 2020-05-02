package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.UnzipAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Unzip(n: Nat,
                       dt1: DataType,
                       dt2: DataType,
                       e: Phrase[ExpType]) extends ExpPrimitive
{
  e :: expT(n`.`(dt1 x dt2), read)
  override val t: ExpType = expT((n`.`dt1) x (n`.`dt2), read)

  override def fedeTranslation(env: Predef.Map[Identifier[ExpType],
                               Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]
                              ): Phrase[AccType] =
    TranslationToImperative.fedAcc(env)(e)(fun(accT(C.t.inT.dataType))(o =>
      UnzipAcc(n, dt1, dt2, C(o))))

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    TranslationToImperative.acc(e)(UnzipAcc(n, dt1, dt2, A))

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(e)(λ(expT(n`.`(dt1 x dt2), read))(x =>
      C(Unzip(n, dt1, dt2, x)) ))

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, e) match {
      case ArrayData(xs) =>
        val (lhs, rhs) = xs.foldLeft((Vector[Data](), Vector[Data]())){
          case (vs: (Vector[Data], Vector[Data]), p: PairData) =>
            (vs._1 :+ p.fst, vs._2 :+ p.snd)
          case _ => throw new Exception("This should not happen")
        }
        PairData(ArrayData(lhs), ArrayData(rhs))

      case _ => throw new Exception("This should not happen")
    }
  }
}
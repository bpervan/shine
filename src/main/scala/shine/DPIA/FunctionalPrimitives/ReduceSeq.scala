package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.IntermediatePrimitives.ReduceSeqI
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ReduceSeq(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType ->: ExpType ->: ExpType],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType],
                           unroll: Boolean = false) extends ExpPrimitive
{
  f :: expT(dt2, read) ->: expT(dt1, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(dt2, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    TranslationToImperative.con(this)(λ(expT(dt2, write))(r =>
      TranslationToImperative.acc(r)(A)))

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(array)(λ(expT(n`.`dt1, read))(X =>
      TranslationToImperative.con(init)(λ(expT(dt2, read))(Y =>
        ReduceSeqI(n, dt1, dt2,
          λ(expT(dt2, read))(x => λ(expT(dt1, read))(y => λ(accT(dt2))(o =>
            TranslationToImperative.acc( f(x)(y) )( o ) ))),
          Y, X, C, unroll)))))

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val initE = OperationalSemantics.eval(s, init)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(Vector(xs.fold(initE) {
          (x, y) => OperationalSemantics.eval(s,
            fE(Literal(x))(Literal(y)))
        }))
      case _ => throw new Exception("This should not happen")
    }
  }
}

